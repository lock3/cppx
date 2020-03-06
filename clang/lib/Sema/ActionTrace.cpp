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
  Out << "New Scope support flags\n";
  #define WRITE_SUPPORTED_FLAG(FLAG_NAME)\
    if(S->getFlags() & clang::Scope::FLAG_NAME) {\
      WriteIndent();\
      Out << "      " << #FLAG_NAME << "\n";\
    }
  WRITE_SUPPORTED_FLAG(FnScope);
  WRITE_SUPPORTED_FLAG(BreakScope);
  WRITE_SUPPORTED_FLAG(ContinueScope);
  WRITE_SUPPORTED_FLAG(DeclScope);
  WRITE_SUPPORTED_FLAG(ControlScope);
  WRITE_SUPPORTED_FLAG(ClassScope);
  WRITE_SUPPORTED_FLAG(BlockScope);
  WRITE_SUPPORTED_FLAG(TemplateParamScope);
  WRITE_SUPPORTED_FLAG(FunctionPrototypeScope);
  WRITE_SUPPORTED_FLAG(FunctionDeclarationScope);
  WRITE_SUPPORTED_FLAG(AtCatchScope);
  WRITE_SUPPORTED_FLAG(ObjCMethodScope);
  WRITE_SUPPORTED_FLAG(SwitchScope);
  WRITE_SUPPORTED_FLAG(TryScope);
  WRITE_SUPPORTED_FLAG(FnTryCatchScope);
  WRITE_SUPPORTED_FLAG(OpenMPDirectiveScope);
  WRITE_SUPPORTED_FLAG(OpenMPLoopDirectiveScope);
  WRITE_SUPPORTED_FLAG(OpenMPSimdDirectiveScope);
  WRITE_SUPPORTED_FLAG(EnumScope);
  WRITE_SUPPORTED_FLAG(SEHTryScope);
  WRITE_SUPPORTED_FLAG(SEHExceptScope);
  WRITE_SUPPORTED_FLAG(SEHFilterScope);
  WRITE_SUPPORTED_FLAG(CompoundStmtScope);
  WRITE_SUPPORTED_FLAG(ClassInheritanceScope);
  WRITE_SUPPORTED_FLAG(CatchScope);
  WRITE_SUPPORTED_FLAG(FragmentScope);

  llvm::outs() << "\n";
  ++Depth;
}

void ActionTrace::LeaveScopeLog(clang::Scope* S) {
  --Depth;
  WriteIndent();
  llvm::raw_ostream &Out
                   = llvm::WithColor(llvm::outs(), llvm::HighlightColor::Macro);
  WriteIndent();
  S->dumpImpl(Out);
  llvm::outs() << "\n";
  Out << "Leaving Scope support flags\n";
  #define WRITE_SUPPORTED_FLAG(FLAG_NAME)\
    if(S->getFlags() & clang::Scope::FLAG_NAME) {\
      WriteIndent();\
      Out << "      " << #FLAG_NAME << "\n";\
    }
  WRITE_SUPPORTED_FLAG(FnScope);
  WRITE_SUPPORTED_FLAG(BreakScope);
  WRITE_SUPPORTED_FLAG(ContinueScope);
  WRITE_SUPPORTED_FLAG(DeclScope);
  WRITE_SUPPORTED_FLAG(ControlScope);
  WRITE_SUPPORTED_FLAG(ClassScope);
  WRITE_SUPPORTED_FLAG(BlockScope);
  WRITE_SUPPORTED_FLAG(TemplateParamScope);
  WRITE_SUPPORTED_FLAG(FunctionPrototypeScope);
  WRITE_SUPPORTED_FLAG(FunctionDeclarationScope);
  WRITE_SUPPORTED_FLAG(AtCatchScope);
  WRITE_SUPPORTED_FLAG(ObjCMethodScope);
  WRITE_SUPPORTED_FLAG(SwitchScope);
  WRITE_SUPPORTED_FLAG(TryScope);
  WRITE_SUPPORTED_FLAG(FnTryCatchScope);
  WRITE_SUPPORTED_FLAG(OpenMPDirectiveScope);
  WRITE_SUPPORTED_FLAG(OpenMPLoopDirectiveScope);
  WRITE_SUPPORTED_FLAG(OpenMPSimdDirectiveScope);
  WRITE_SUPPORTED_FLAG(EnumScope);
  WRITE_SUPPORTED_FLAG(SEHTryScope);
  WRITE_SUPPORTED_FLAG(SEHExceptScope);
  WRITE_SUPPORTED_FLAG(SEHFilterScope);
  WRITE_SUPPORTED_FLAG(CompoundStmtScope);
  WRITE_SUPPORTED_FLAG(ClassInheritanceScope);
  WRITE_SUPPORTED_FLAG(CatchScope);
  WRITE_SUPPORTED_FLAG(FragmentScope);

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