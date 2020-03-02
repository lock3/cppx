#include "clang/Sema/ActionTrace.h"
#include "llvm/Support/WithColor.h"

namespace sematrace {

int ActionTrace::Depth = 0;
const char* ActionTrace::Indent = "  ";



void ActionTrace::WriteIndent() {
  for(int I = 0; I < Depth; ++I) 
    llvm::outs() << Indent;
}

// static void (llvm::StringRef FunctionName, llvm::StringRef Msg = "");
// static 
void ActionTrace::EnterSemaCall(llvm::StringRef FunctionName, llvm::StringRef Msg) {
  WriteIndent();
  llvm::WithColor(llvm::outs(), llvm::HighlightColor::Remark) << "Called Sema::" << FunctionName;
  if(Msg != "") {
    llvm::WithColor(llvm::outs(), llvm::HighlightColor::Remark) << ". Message: " << Msg;
  }
  llvm::outs() << "\n";
  ++Depth;
}
void ActionTrace::LeavingSemaCall(llvm::StringRef FunctionName, llvm::StringRef Msg) {
  --Depth;
  WriteIndent();
  llvm::WithColor(llvm::outs(), llvm::HighlightColor::Remark) << "Leaving Sema::" << FunctionName;
  if(Msg != "") {
    llvm::WithColor(llvm::outs(), llvm::HighlightColor::Remark) << ". Message: " << Msg;
  }
  llvm::outs() << "\n";
}

void ActionTrace::EnterParsingFn(llvm::StringRef FunctionName, llvm::StringRef Msg) {
  WriteIndent();
  llvm::WithColor(llvm::outs(), llvm::HighlightColor::Macro) << "Entering Parser::" << FunctionName;
  if(Msg != "") {
    llvm::WithColor(llvm::outs(), llvm::HighlightColor::Macro) << ". Message: " << Msg;
  }
  llvm::outs() << "\n";
  ++Depth;
}

void ActionTrace::LeavingParsingFn(llvm::StringRef FunctionName, llvm::StringRef Msg) {
  --Depth;
  WriteIndent();
  llvm::WithColor(llvm::outs(), llvm::HighlightColor::Macro) << "Leaving Parser::" << FunctionName;
  if(Msg != "") {
    llvm::WithColor(llvm::outs(), llvm::HighlightColor::Macro) << ". Message: " << Msg;
  }
  llvm::outs() << "\n";
}

ActionLoggingGuard::ActionLoggingGuard(llvm::StringRef FnName)
  :FunctionName(FnName)
{
  ActionTrace::EnterParsingFn(FunctionName);
}

ActionLoggingGuard::~ActionLoggingGuard() {
  ActionTrace::LeavingParsingFn(FunctionName);
}

SemaActionLoggingGuard::SemaActionLoggingGuard(llvm::StringRef FnName)
  :FunctionName(FnName)
{
  ActionTrace::EnterSemaCall(FunctionName);
}
SemaActionLoggingGuard::~SemaActionLoggingGuard() {
  ActionTrace::LeavingSemaCall(FunctionName);
}

}