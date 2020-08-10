//===- unittest/Gold/GoldCompileRun.cpp -----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "GoldCompileRun.h"
#include "clang/Gold/ParseGoldAST.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Parse/ParseAST.h"
#include "clang/Sema/Sema.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MemoryBuffer.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "clang/Frontend/CompilerInstance.h"

namespace gold {

static void ErrorAndExit(std::string message) {
  llvm::errs()<< "ERROR: " << message << "\n";
  std::exit(1);
}

testing::AssertionResult CompileGoldCode(llvm::LLVMContext &Context,
   llvm::StringRef Code, std::unique_ptr<llvm::ExecutionEngine> &EE) {

  using namespace llvm;
  using namespace clang;
  InitializeNativeTarget();
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();
  CompilerInstance compiler;

  compiler.createDiagnostics();
  compiler.getLangOpts().CPlusPlus = 1;
  compiler.getLangOpts().CPlusPlus11 = 1;
  compiler.getLangOpts().CPlusPlus14 = 1;
  compiler.getLangOpts().CPlusPlus17 = 1;
  compiler.getLangOpts().Gold = 1;

  compiler.getTargetOpts().Triple = llvm::Triple::normalize(
      llvm::sys::getProcessTriple());
  compiler.setTarget(clang::TargetInfo::CreateTargetInfo(
    compiler.getDiagnostics(),
    std::make_shared<clang::TargetOptions>(
      compiler.getTargetOpts())));

  compiler.createFileManager();
  compiler.createSourceManager(compiler.getFileManager());
  compiler.createPreprocessor(clang::TU_Prefix);

  compiler.createASTContext();
  CodeGenerator* Generator;
  compiler.setASTConsumer(std::unique_ptr<ASTConsumer>(
      Generator = CreateLLVMCodeGen(compiler.getDiagnostics(),
          "test.usyntax", compiler.getHeaderSearchOpts(),
          compiler.getPreprocessorOpts(), compiler.getCodeGenOpts(), Context)));

  compiler.createSema(clang::TU_Prefix, nullptr);

  clang::SourceManager &sm = compiler.getSourceManager();
  sm.setMainFileID(sm.createFileID(
    llvm::MemoryBuffer::getMemBuffer(Code), clang::SrcMgr::C_User));
  ParseGoldAST(compiler.getASTContext(), compiler.getPreprocessor(),
    compiler.getSema());
  std::unique_ptr<llvm::Module> Owner(Generator->ReleaseModule());
  if(!Owner) {
    llvm::outs() << "We didn't retrieve the module from C++ land.\n";
    return testing::AssertionFailure();
  }

  EngineBuilder EB(std::move(Owner));
  EB.setMArch("");
  EB.setMCPU(sys::getHostCPUName());
  std::string Error;
  EB.setErrorStr(&Error);
  EB.setEngineKind(EngineKind::JIT);
  std::unique_ptr<RTDyldMemoryManager> MM(new SectionMemoryManager);
  EB.setMCJITMemoryManager(std::move(MM));
  EB.setOptLevel(CodeGenOpt::None);
  // EB.setTargetOptions(compiler.getTargetOpts());

  EE.reset(EB.create());
  if (!EE)
    ErrorAndExit("Could not create execution engine");

  // FIXME: Do we need to set the builder's MAttrs?
  // EE.reset(EB.setEngineKind(EngineKind::JIT)
  //               .setMCJITMemoryManager(std::move(MM))
  //               .setErrorStr(&Error)
  //               .setOptLevel(CodeGenOpt::None)
  //               .setMArch("")
  //               .setMCPU(sys::getHostCPUName())
  //               //.setMAttrs(MAttrs)
  //               .create());
  // if(!EE) {
  //   llvm::outs() <<"Failed to create Execution engine.\n";
  //   llvm::outs() << "Reason: " << Error << "\n";
  //   return testing::AssertionFailure();
  // }

  EE->finalizeObject();
  EE->runStaticConstructorsDestructors(true);
  return testing::AssertionSuccess();
}

}
