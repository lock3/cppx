#ifndef LLVM_CLANG_SEMA_ACTION_TRACE_H
#define LLVM_CLANG_SEMA_ACTION_TRACE_H
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringRef.h"

namespace sema {

class ActionTrace {
  static int Depth;
  static const char* Indent;
public:

  static void WriteIndent();
  static void WriteSemaCall(llvm::StringRef FunctionName, llvm::StringRef Msg = "");
  static void EnterParsingFn(llvm::StringRef FunctionName, llvm::StringRef Msg = "");
  static void LeavingParsingFn(llvm::StringRef FunctionName, llvm::StringRef Msg = "");
};

struct ActionLoggingGuard {
  ActionLoggingGuard(llvm::StringRef FnName);
  ~ActionLoggingGuard();
  llvm::StringRef FunctionName;
};
#define PARSER_ENTER_TRACE()\
  ::sema::ActionTrace::EnterParsingFn(__FUNCTION__);

#define PARSER_ENTER_TRACE_M(MSG)\
  ::sema::ActionTrace::EnterParsingFn(__FUNCTION__, MSG);

#define PARSER_LEAVE_TRACE()\
  ::sema::ActionTrace::LeavingParsingFn(__FUNCTION__);

#define PARSER_LEAVE_TRACE_M(MSG)\
  ::sema::ActionTrace::LeavingParsingFn(__FUNCTION__, MSG);

#define SEMA_TRACE()\
  ::sema::ActionTrace::WriteSemaCall(__FUNCTION__);

#define SEMA_TRACE_M(MSG)\
  ::sema::ActionTrace::WriteSemaCall(__FUNCTION__, MSG);

#define PARSING_LOG()\
  ::sema::ActionLoggingGuard GardTemp(__FUNCTION__)
}

#endif