#ifndef LLVM_CLANG_SEMA_ACTION_TRACE_H
#define LLVM_CLANG_SEMA_ACTION_TRACE_H
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringRef.h"

namespace clang {
  class Scope;
}

namespace sematrace {

class ActionTrace {
  static int Depth;
  static const char* Indent;
public:

  static void WriteIndent();
  static void EnterSemaCall(llvm::StringRef FunctionName, llvm::StringRef Msg = "");
  static void LeavingSemaCall(llvm::StringRef FunctionName, llvm::StringRef Msg = "");
  static void EnterParsingFn(llvm::StringRef FunctionName, llvm::StringRef Msg = "");
  static void LeavingParsingFn(llvm::StringRef FunctionName, llvm::StringRef Msg = "");
  static void EnterScopeLog(clang::Scope* S);
  static void LeaveScopeLog(clang::Scope* S);
};

struct ActionLoggingGuard {
  ActionLoggingGuard(llvm::StringRef FnName);
  ~ActionLoggingGuard();
  llvm::StringRef FunctionName;
};
struct SemaActionLoggingGuard {
  SemaActionLoggingGuard(llvm::StringRef FnName);
  ~SemaActionLoggingGuard();
  llvm::StringRef FunctionName;
};

#define PARSER_ENTER_TRACE()\
  ::sematrace::ActionTrace::EnterParsingFn(__FUNCTION__);

#define PARSER_ENTER_TRACE_M(MSG)\
  ::sematrace::ActionTrace::EnterParsingFn(__FUNCTION__, MSG);

#define PARSER_LEAVE_TRACE()\
  ::sematrace::ActionTrace::LeavingParsingFn(__FUNCTION__);

#define PARSER_LEAVE_TRACE_M(MSG)\
  ::sematrace::ActionTrace::LeavingParsingFn(__FUNCTION__, MSG);

#define PARSING_LOG()\
  ::sematrace::ActionLoggingGuard GardTemp(__FUNCTION__)

#define SEMA_LOG()\
  ::sematrace::SemaActionLoggingGuard GardTemp(__FUNCTION__)
}

#endif