//===- DebugTypes.cpp -----------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "DebugTypes.h"
#include "Chunks.h"
#include "Driver.h"
#include "InputFiles.h"
#include "TypeMerger.h"
#include "lld/Common/ErrorHandler.h"
#include "lld/Common/Memory.h"
#include "llvm/DebugInfo/CodeView/TypeRecord.h"
#include "llvm/DebugInfo/CodeView/TypeRecordHelpers.h"
#include "llvm/DebugInfo/CodeView/TypeStreamMerger.h"
#include "llvm/DebugInfo/PDB/GenericError.h"
#include "llvm/DebugInfo/PDB/Native/InfoStream.h"
#include "llvm/DebugInfo/PDB/Native/NativeSession.h"
#include "llvm/DebugInfo/PDB/Native/PDBFile.h"
#include "llvm/DebugInfo/PDB/Native/TpiStream.h"
#include "llvm/Support/Path.h"

using namespace llvm;
using namespace llvm::codeview;
using namespace lld;
using namespace lld::coff;

namespace {
class TypeServerIpiSource;

// The TypeServerSource class represents a PDB type server, a file referenced by
// OBJ files compiled with MSVC /Zi. A single PDB can be shared by several OBJ
// files, therefore there must be only once instance per OBJ lot. The file path
// is discovered from the dependent OBJ's debug type stream. The
// TypeServerSource object is then queued and loaded by the COFF Driver. The
// debug type stream for such PDB files will be merged first in the final PDB,
// before any dependent OBJ.
class TypeServerSource : public TpiSource {
public:
  explicit TypeServerSource(PDBInputFile *f)
      : TpiSource(PDB, nullptr), pdbInputFile(f) {
    if (f->loadErr && *f->loadErr)
      return;
    pdb::PDBFile &file = f->session->getPDBFile();
    auto expectedInfo = file.getPDBInfoStream();
    if (!expectedInfo)
      return;
    auto it = mappings.emplace(expectedInfo->getGuid(), this);
    assert(it.second);
    (void)it;
  }

  Error mergeDebugT(TypeMerger *m) override;
  bool isDependency() const override { return true; }

  PDBInputFile *pdbInputFile = nullptr;

  // TpiSource for IPI stream.
  TypeServerIpiSource *ipiSrc = nullptr;

  static std::map<codeview::GUID, TypeServerSource *> mappings;
};

// Companion to TypeServerSource. Stores the index map for the IPI stream in the
// PDB. Modeling PDBs with two sources for TPI and IPI helps establish the
// invariant of one type index space per source.
class TypeServerIpiSource : public TpiSource {
public:
  explicit TypeServerIpiSource() : TpiSource(PDBIpi, nullptr) {}

  friend class TypeServerSource;

  // IPI merging is handled in TypeServerSource::mergeDebugT, since it depends
  // directly on type merging.
  Error mergeDebugT(TypeMerger *m) override { return Error::success(); }

  bool isDependency() const override { return true; }
};

// This class represents the debug type stream of an OBJ file that depends on a
// PDB type server (see TypeServerSource).
class UseTypeServerSource : public TpiSource {
public:
  UseTypeServerSource(ObjFile *f, TypeServer2Record ts)
      : TpiSource(UsingPDB, f), typeServerDependency(ts) {}

  Error mergeDebugT(TypeMerger *m) override;

  // Information about the PDB type server dependency, that needs to be loaded
  // in before merging this OBJ.
  TypeServer2Record typeServerDependency;
};

// This class represents the debug type stream of a Microsoft precompiled
// headers OBJ (PCH OBJ). This OBJ kind needs to be merged first in the output
// PDB, before any other OBJs that depend on this. Note that only MSVC generate
// such files, clang does not.
class PrecompSource : public TpiSource {
public:
  PrecompSource(ObjFile *f) : TpiSource(PCH, f) {
    if (!f->pchSignature || !*f->pchSignature)
      fatal(toString(f) +
            " claims to be a PCH object, but does not have a valid signature");
    auto it = mappings.emplace(*f->pchSignature, this);
    if (!it.second)
      fatal("a PCH object with the same signature has already been provided (" +
            toString(it.first->second->file) + " and " + toString(file) + ")");
  }

  bool isDependency() const override { return true; }

  static std::map<uint32_t, PrecompSource *> mappings;
};

// This class represents the debug type stream of an OBJ file that depends on a
// Microsoft precompiled headers OBJ (see PrecompSource).
class UsePrecompSource : public TpiSource {
public:
  UsePrecompSource(ObjFile *f, PrecompRecord precomp)
      : TpiSource(UsingPCH, f), precompDependency(precomp) {}

  Error mergeDebugT(TypeMerger *m) override;

  // Information about the Precomp OBJ dependency, that needs to be loaded in
  // before merging this OBJ.
  PrecompRecord precompDependency;
};
} // namespace

static std::vector<TpiSource *> gc;

TpiSource::TpiSource(TpiKind k, ObjFile *f) : kind(k), file(f) {
  gc.push_back(this);
}

// Vtable key method.
TpiSource::~TpiSource() = default;

TpiSource *lld::coff::makeTpiSource(ObjFile *file) {
  return make<TpiSource>(TpiSource::Regular, file);
}

TpiSource *lld::coff::makeTypeServerSource(PDBInputFile *pdbInputFile) {
  // Type server sources come in pairs: the TPI stream, and the IPI stream.
  auto *tpiSource = make<TypeServerSource>(pdbInputFile);
  if (pdbInputFile->session->getPDBFile().hasPDBIpiStream())
    tpiSource->ipiSrc = make<TypeServerIpiSource>();
  return tpiSource;
}

TpiSource *lld::coff::makeUseTypeServerSource(ObjFile *file,
                                              TypeServer2Record ts) {
  return make<UseTypeServerSource>(file, ts);
}

TpiSource *lld::coff::makePrecompSource(ObjFile *file) {
  return make<PrecompSource>(file);
}

TpiSource *lld::coff::makeUsePrecompSource(ObjFile *file,
                                           PrecompRecord precomp) {
  return make<UsePrecompSource>(file, precomp);
}

void TpiSource::forEachSource(llvm::function_ref<void(TpiSource *)> fn) {
  for_each(gc, fn);
}

std::map<codeview::GUID, TypeServerSource *> TypeServerSource::mappings;

std::map<uint32_t, PrecompSource *> PrecompSource::mappings;

// A COFF .debug$H section is currently a clang extension.  This function checks
// if a .debug$H section is in a format that we expect / understand, so that we
// can ignore any sections which are coincidentally also named .debug$H but do
// not contain a format we recognize.
static bool canUseDebugH(ArrayRef<uint8_t> debugH) {
  if (debugH.size() < sizeof(object::debug_h_header))
    return false;
  auto *header =
      reinterpret_cast<const object::debug_h_header *>(debugH.data());
  debugH = debugH.drop_front(sizeof(object::debug_h_header));
  return header->Magic == COFF::DEBUG_HASHES_SECTION_MAGIC &&
         header->Version == 0 &&
         header->HashAlgorithm == uint16_t(GlobalTypeHashAlg::SHA1_8) &&
         (debugH.size() % 8 == 0);
}

static Optional<ArrayRef<uint8_t>> getDebugH(ObjFile *file) {
  SectionChunk *sec =
      SectionChunk::findByName(file->getDebugChunks(), ".debug$H");
  if (!sec)
    return llvm::None;
  ArrayRef<uint8_t> contents = sec->getContents();
  if (!canUseDebugH(contents))
    return None;
  return contents;
}

static ArrayRef<GloballyHashedType>
getHashesFromDebugH(ArrayRef<uint8_t> debugH) {
  assert(canUseDebugH(debugH));

  debugH = debugH.drop_front(sizeof(object::debug_h_header));
  uint32_t count = debugH.size() / sizeof(GloballyHashedType);
  return {reinterpret_cast<const GloballyHashedType *>(debugH.data()), count};
}

// Merge .debug$T for a generic object file.
Error TpiSource::mergeDebugT(TypeMerger *m) {
  CVTypeArray types;
  BinaryStreamReader reader(file->debugTypes, support::little);
  cantFail(reader.readArray(types, reader.getLength()));

  if (config->debugGHashes) {
    ArrayRef<GloballyHashedType> hashes;
    std::vector<GloballyHashedType> ownedHashes;
    if (Optional<ArrayRef<uint8_t>> debugH = getDebugH(file))
      hashes = getHashesFromDebugH(*debugH);
    else {
      ownedHashes = GloballyHashedType::hashTypes(types);
      hashes = ownedHashes;
    }

    if (auto err = mergeTypeAndIdRecords(m->globalIDTable, m->globalTypeTable,
                                         indexMapStorage, types, hashes,
                                         file->pchSignature))
      fatal("codeview::mergeTypeAndIdRecords failed: " +
            toString(std::move(err)));
  } else {
    if (auto err =
            mergeTypeAndIdRecords(m->idTable, m->typeTable, indexMapStorage,
                                  types, file->pchSignature))
      fatal("codeview::mergeTypeAndIdRecords failed: " +
            toString(std::move(err)));
  }

  // In an object, there is only one mapping for both types and items.
  tpiMap = indexMapStorage;
  ipiMap = indexMapStorage;

  if (config->showSummary) {
    // Count how many times we saw each type record in our input. This
    // calculation requires a second pass over the type records to classify each
    // record as a type or index. This is slow, but this code executes when
    // collecting statistics.
    m->tpiCounts.resize(m->getTypeTable().size());
    m->ipiCounts.resize(m->getIDTable().size());
    uint32_t srcIdx = 0;
    for (CVType &ty : types) {
      TypeIndex dstIdx = tpiMap[srcIdx++];
      // Type merging may fail, so a complex source type may become the simple
      // NotTranslated type, which cannot be used as an array index.
      if (dstIdx.isSimple())
        continue;
      SmallVectorImpl<uint32_t> &counts =
          isIdRecord(ty.kind()) ? m->ipiCounts : m->tpiCounts;
      ++counts[dstIdx.toArrayIndex()];
    }
  }

  return Error::success();
}

// Merge types from a type server PDB.
Error TypeServerSource::mergeDebugT(TypeMerger *m) {
  pdb::PDBFile &pdbFile = pdbInputFile->session->getPDBFile();
  Expected<pdb::TpiStream &> expectedTpi = pdbFile.getPDBTpiStream();
  if (auto e = expectedTpi.takeError())
    fatal("Type server does not have TPI stream: " + toString(std::move(e)));
  pdb::TpiStream *maybeIpi = nullptr;
  if (pdbFile.hasPDBIpiStream()) {
    Expected<pdb::TpiStream &> expectedIpi = pdbFile.getPDBIpiStream();
    if (auto e = expectedIpi.takeError())
      fatal("Error getting type server IPI stream: " + toString(std::move(e)));
    maybeIpi = &*expectedIpi;
  }

  if (config->debugGHashes) {
    // PDBs do not actually store global hashes, so when merging a type server
    // PDB we have to synthesize global hashes.  To do this, we first synthesize
    // global hashes for the TPI stream, since it is independent, then we
    // synthesize hashes for the IPI stream, using the hashes for the TPI stream
    // as inputs.
    auto tpiHashes = GloballyHashedType::hashTypes(expectedTpi->typeArray());
    Optional<uint32_t> endPrecomp;
    // Merge TPI first, because the IPI stream will reference type indices.
    if (auto err =
            mergeTypeRecords(m->globalTypeTable, indexMapStorage,
                             expectedTpi->typeArray(), tpiHashes, endPrecomp))
      fatal("codeview::mergeTypeRecords failed: " + toString(std::move(err)));
    tpiMap = indexMapStorage;

    // Merge IPI.
    if (maybeIpi) {
      auto ipiHashes =
          GloballyHashedType::hashIds(maybeIpi->typeArray(), tpiHashes);
      if (auto err =
              mergeIdRecords(m->globalIDTable, tpiMap, ipiSrc->indexMapStorage,
                             maybeIpi->typeArray(), ipiHashes))
        fatal("codeview::mergeIdRecords failed: " + toString(std::move(err)));
      ipiMap = ipiSrc->indexMapStorage;
    }
  } else {
    // Merge TPI first, because the IPI stream will reference type indices.
    if (auto err = mergeTypeRecords(m->typeTable, indexMapStorage,
                                    expectedTpi->typeArray()))
      fatal("codeview::mergeTypeRecords failed: " + toString(std::move(err)));
    tpiMap = indexMapStorage;

    // Merge IPI.
    if (maybeIpi) {
      if (auto err = mergeIdRecords(m->idTable, tpiMap, ipiSrc->indexMapStorage,
                                    maybeIpi->typeArray()))
        fatal("codeview::mergeIdRecords failed: " + toString(std::move(err)));
      ipiMap = ipiSrc->indexMapStorage;
    }
  }

  if (config->showSummary) {
    // Count how many times we saw each type record in our input. If a
    // destination type index is present in the source to destination type index
    // map, that means we saw it once in the input. Add it to our histogram.
    m->tpiCounts.resize(m->getTypeTable().size());
    m->ipiCounts.resize(m->getIDTable().size());
    for (TypeIndex ti : tpiMap)
      if (!ti.isSimple())
        ++m->tpiCounts[ti.toArrayIndex()];
    for (TypeIndex ti : ipiMap)
      if (!ti.isSimple())
        ++m->ipiCounts[ti.toArrayIndex()];
  }

  return Error::success();
}

Error UseTypeServerSource::mergeDebugT(TypeMerger *m) {
  const codeview::GUID &tsId = typeServerDependency.getGuid();
  StringRef tsPath = typeServerDependency.getName();

  TypeServerSource *tsSrc;
  auto it = TypeServerSource::mappings.find(tsId);
  if (it != TypeServerSource::mappings.end()) {
    tsSrc = it->second;
  } else {
    // The file failed to load, lookup by name
    PDBInputFile *pdb = PDBInputFile::findFromRecordPath(tsPath, file);
    if (!pdb)
      return createFileError(tsPath, errorCodeToError(std::error_code(
                                         ENOENT, std::generic_category())));
    // If an error occurred during loading, throw it now
    if (pdb->loadErr && *pdb->loadErr)
      return createFileError(tsPath, std::move(*pdb->loadErr));

    tsSrc = (TypeServerSource *)pdb->debugTypesObj;
  }

  pdb::PDBFile &pdbSession = tsSrc->pdbInputFile->session->getPDBFile();
  auto expectedInfo = pdbSession.getPDBInfoStream();
  if (!expectedInfo)
    return expectedInfo.takeError();

  // Just because a file with a matching name was found and it was an actual
  // PDB file doesn't mean it matches.  For it to match the InfoStream's GUID
  // must match the GUID specified in the TypeServer2 record.
  if (expectedInfo->getGuid() != typeServerDependency.getGuid())
    return createFileError(
        tsPath,
        make_error<pdb::PDBError>(pdb::pdb_error_code::signature_out_of_date));

  // Reuse the type index map of the type server.
  tpiMap = tsSrc->tpiMap;
  ipiMap = tsSrc->ipiMap;
  return Error::success();
}

static bool equalsPath(StringRef path1, StringRef path2) {
#if defined(_WIN32)
  return path1.equals_lower(path2);
#else
  return path1.equals(path2);
#endif
}

// Find by name an OBJ provided on the command line
static PrecompSource *findObjByName(StringRef fileNameOnly) {
  SmallString<128> currentPath;
  for (auto kv : PrecompSource::mappings) {
    StringRef currentFileName = sys::path::filename(kv.second->file->getName(),
                                                    sys::path::Style::windows);

    // Compare based solely on the file name (link.exe behavior)
    if (equalsPath(currentFileName, fileNameOnly))
      return kv.second;
  }
  return nullptr;
}

static Expected<PrecompSource *> findPrecompMap(ObjFile *file,
                                                PrecompRecord &pr) {
  // Cross-compile warning: given that Clang doesn't generate LF_PRECOMP
  // records, we assume the OBJ comes from a Windows build of cl.exe. Thusly,
  // the paths embedded in the OBJs are in the Windows format.
  SmallString<128> prFileName =
      sys::path::filename(pr.getPrecompFilePath(), sys::path::Style::windows);

  PrecompSource *precomp;
  auto it = PrecompSource::mappings.find(pr.getSignature());
  if (it != PrecompSource::mappings.end()) {
    precomp = it->second;
  } else {
    // Lookup by name
    precomp = findObjByName(prFileName);
  }

  if (!precomp)
    return createFileError(
        prFileName,
        make_error<pdb::PDBError>(pdb::pdb_error_code::no_matching_pch));

  if (pr.getSignature() != file->pchSignature)
    return createFileError(
        toString(file),
        make_error<pdb::PDBError>(pdb::pdb_error_code::no_matching_pch));

  if (pr.getSignature() != *precomp->file->pchSignature)
    return createFileError(
        toString(precomp->file),
        make_error<pdb::PDBError>(pdb::pdb_error_code::no_matching_pch));

  return precomp;
}

/// Merges a precompiled headers TPI map into the current TPI map. The
/// precompiled headers object will also be loaded and remapped in the
/// process.
static Error
mergeInPrecompHeaderObj(ObjFile *file,
                        SmallVectorImpl<TypeIndex> &indexMapStorage,
                        PrecompRecord &precomp) {
  auto e = findPrecompMap(file, precomp);
  if (!e)
    return e.takeError();

  PrecompSource *precompSrc = *e;
  if (precompSrc->tpiMap.empty())
    return Error::success();

  assert(precomp.getStartTypeIndex() == TypeIndex::FirstNonSimpleIndex);
  assert(precomp.getTypesCount() <= precompSrc->tpiMap.size());
  // Use the previously remapped index map from the precompiled headers.
  indexMapStorage.append(precompSrc->tpiMap.begin(),
                         precompSrc->tpiMap.begin() + precomp.getTypesCount());
  return Error::success();
}

Error UsePrecompSource::mergeDebugT(TypeMerger *m) {
  // This object was compiled with /Yu, so process the corresponding
  // precompiled headers object (/Yc) first. Some type indices in the current
  // object are referencing data in the precompiled headers object, so we need
  // both to be loaded.
  if (Error e =
          mergeInPrecompHeaderObj(file, indexMapStorage, precompDependency))
    return e;

  return TpiSource::mergeDebugT(m);
}

uint32_t TpiSource::countTypeServerPDBs() {
  return TypeServerSource::mappings.size();
}

uint32_t TpiSource::countPrecompObjs() {
  return PrecompSource::mappings.size();
}

void TpiSource::clear() {
  gc.clear();
  TypeServerSource::mappings.clear();
  PrecompSource::mappings.clear();
}
