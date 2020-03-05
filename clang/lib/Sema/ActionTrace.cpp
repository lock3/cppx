#include "clang/Sema/ActionTrace.h"
#include "llvm/Support/WithColor.h"
#include "clang/Sema/Scope.h"


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

void ActionTrace::EnterScopeLog(clang::Scope* S) {
  WriteIndent();
  llvm::raw_ostream &Out
                   = llvm::WithColor(llvm::outs(), llvm::HighlightColor::Macro);
  Out << "New Scope support flags";
  #define WRITE_SUPPORTED_FLAG(FLAG_NAME)\
    if(S->getFlags() & FLAG_NAME) {\
      WriteIndent();\
      Out << "      " << #FLAG_NAME << "\n";\
    }
  WRITE_SUPPORTED_FLAG(clang::Scope::FnScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::BreakScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::ContinueScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::DeclScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::ControlScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::ClassScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::BlockScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::TemplateParamScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::FunctionPrototypeScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::FunctionDeclarationScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::AtCatchScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::ObjCMethodScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::SwitchScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::TryScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::FnTryCatchScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::OpenMPDirectiveScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::OpenMPLoopDirectiveScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::OpenMPSimdDirectiveScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::EnumScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::SEHTryScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::SEHExceptScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::SEHFilterScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::CompoundStmtScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::ClassInheritanceScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::CatchScope);
  WRITE_SUPPORTED_FLAG(clang::Scope::FragmentScope);

  llvm::outs() << "\n";
  ++Depth;
}

void ActionTrace::LeaveScopeLog(clang::Scope* S) {

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