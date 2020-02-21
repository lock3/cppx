#include "clang/Sema/ActionTrace.h"


namespace Sema {

int ActionTrace::Depth = 0;
const char* ActionTrace::Indent = "  ";



void ActionTrace::WriteIndent() {
  for(int I = 0; I < Depth; ++I) 
    llvm::outs() << Indent;
}

void ActionTrace::WriteSemaCall(llvm::StringRef FunctionName, llvm::StringRef Msg) {
  WriteIndent();
  llvm::outs() << "Called Sema::" << FunctionName;
  if(Msg != "") {
    llvm::outs() << ". Message: " << Msg;
  }
  llvm::outs() << "\n";

}

void ActionTrace::EnterParsingFn(llvm::StringRef FunctionName, llvm::StringRef Msg) {
  WriteIndent();
  llvm::outs() << "Entering Parser::" << FunctionName;
  if(Msg != "") {
    llvm::outs() << ". Message: " << Msg;
  }
  llvm::outs() << "\n";
  ++Depth;
}

void ActionTrace::LeavingParsingFn(llvm::StringRef FunctionName, llvm::StringRef Msg) {
  WriteIndent();
  llvm::outs() << "Leaving Parser::" << FunctionName;
  if(Msg != "") {
    llvm::outs() << ". Message: " << Msg;
  }
  llvm::outs() << "\n";
  --Depth;
}

ActionLoggingGuard::ActionLoggingGuard(llvm::StringRef FnName)
  :FunctionName(FnName)
{
  ActionTrace::EnterParsingFn(FunctionName);
}

ActionLoggingGuard::~ActionLoggingGuard() {
  ActionTrace::LeavingParsingFn(FunctionName);
}

}