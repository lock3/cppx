//===- GoldSema.cpp - Semantic Analysis of Gold ASTs ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the gold::Sema class, which performs semantic analysis
//  for the Gold language.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Stmt.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Sema/Lookup.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Gold/GoldSyntax.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldElaborator.h"

namespace gold {

using namespace llvm;

Sema::Sema(SyntaxContext &Context, clang::Sema &CxxSema)
  : CxxSema(CxxSema), CurrentDecl(), Context(Context),
    Diags(Context.CxxAST.getSourceManager().getDiagnostics())
{
  CxxSema.CurScope = nullptr;
  OperatorColonII = &Context.CxxAST.Idents.get("operator':'");
  OperatorExclaimII = &Context.CxxAST.Idents.get("operator'!'");
  OperatorEqualsII = &Context.CxxAST.Idents.get("operator'='");
  OperatorIfII = &Context.CxxAST.Idents.get("operator'if'");
  OperatorElseII = &Context.CxxAST.Idents.get("operator'else'");
  OperatorReturnII = &Context.CxxAST.Idents.get("operator'return'");
  OperatorReturnsII = &Context.CxxAST.Idents.get("operator'returns'");
  OperatorDotII = &Context.CxxAST.Idents.get("operator'.'");
  OperatorForII = &Context.CxxAST.Idents.get("operator'for'");
  OperatorInII = &Context.CxxAST.Idents.get("operator'in'");
}

Sema::~Sema() {
  assert(ScopeStack.size() == 0 && "Scope stack is not empty.");
  delete getCurClangScope();
  CxxSema.CurScope = nullptr;
}

bool Sema::accessSpecifierIsValidInScope() const {
  return ScopeStack.back() && ScopeStack.back()->getKind() == SK_Class;
}

Scope *Sema::getCurrentScope() {
  if (ScopeStack.empty())
    return nullptr;
  return ScopeStack.back();
}

void Sema::pushScope(Scope *S) {
  // FIXME: The scope should self-describe itself. We can't rely on
  // the existence of Clang structures at the time we push a scope.
  // if (S->isDeclarationScope())
  //   CxxSema.PushFunctionScope();

  ScopeStack.push_back(S);
}

Scope *Sema::popScope() {
  Scope *R = ScopeStack.back();
  ScopeStack.pop_back();
  return R;
}

void Sema::enterScope(ScopeKind K, const Syntax *S) {
  // FIXME: We're leaking scopes. We probably want to keep them bound to the
  // syntax for which they're created, especially for syntaxes that correspond
  // to declarations, so that we can easily find their associated lookup
  // tables. See the comments in leaveScope and saveScope.
  //
  // NOTE: Do not allocate this through the Context. It might be deleted.
  pushScope(new Scope(K, S, getCurrentScope()));
}

void Sema::enterScope(clang::CXXRecordDecl* R, const Syntax* S) {
  pushScope(new Scope(SK_Class, S, getCurrentScope(), R));
}

void Sema::leaveScope(const Syntax *S) {
  assert(getCurrentScope()->getConcreteTerm() == S);
  // FIXME: Delete the scope. Note that we don't delete the scope in saveScope.
  popScope();
}

Scope *Sema::saveScope(const Syntax *S) {
  assert(getCurrentScope()->getConcreteTerm() == S);
  // FIXME: Queue the scope for subsequent deletion?
  Scope *Scope = getCurrentScope();
  popScope();
  return Scope;
}

clang::DeclContext *Sema::getCurrentCxxDeclContext() {
  return CurrentDecl->getCxxContext();
}

void Sema::pushDecl(Declaration *D) {
  assert(D->getOwner() == CurrentDecl);
  
  // FIXME: this might be an incorrect assertion.
  assert(D->Cxx && isa<clang::DeclContext>(D->Cxx)
         && "No Cxx declaration to push.");

  CurrentDecl = D;
  getCxxSema().CurContext = clang::Decl::castToDeclContext(D->Cxx);
}

void Sema::setCurrentDecl(Declaration *D) {
  CurrentDecl = D;
}

void Sema::popDecl() {
  CurrentDecl = CurrentDecl->getOwner();
  getCxxSema().CurContext = CurrentDecl ?
    clang::Decl::castToDeclContext(CurrentDecl->Cxx) : nullptr;
}

bool Sema::lookupUnqualifiedName(clang::LookupResult &R) {
  return lookupUnqualifiedName(R, getCurrentScope());
}

bool Sema::lookupUnqualifiedName(clang::LookupResult &R, Scope *S) {
  assert(S);

  clang::DeclarationName Name = R.getLookupName();
  clang::IdentifierInfo *Id = Name.getAsIdentifierInfo();
  assert(Id && "Invalid id");

  clang::Sema::LookupNameKind LookupKind = R.getLookupKind();

  if (LookupKind == clang::Sema::LookupTagName ||
      LookupKind == clang::Sema::LookupAnyName) {
    auto BuiltinMapIter = BuiltinTypes.find(Id->getName());
    if (BuiltinMapIter != BuiltinTypes.end())
      return true;
  }

  while (S) {
    std::set<Declaration *> Found = S->findDecl(Id);
    if (!Found.empty()) {
      for (auto *FoundDecl : Found) {
        // If we find a name that hasn't been elaborated,
        // then we actually need to elaborate it.
        if (!FoundDecl->Cxx) {
          Elaborator(Context, *this).elaborateDeclEarly(FoundDecl);
        }
        if (!FoundDecl->Cxx) {
          // FIXME: This needs a more appropriate error message.
          llvm_unreachable("Early elaboration of declaration failed. Unable to continue.");
        }
        if (!isa<clang::NamedDecl>(FoundDecl->Cxx)) {
            llvm_unreachable("Decl elaboration failure.");
          FoundDecl->Cxx->dump();
        }
        clang::NamedDecl *ND = cast<clang::NamedDecl>(FoundDecl->Cxx);

        // FIXME: check if this is a tag decl, not a type decl!
        if (LookupKind == clang::Sema::LookupTagName &&
            !isa<clang::TypeDecl>(ND)) {
          // FIXME: Give a proper diagnostic once we implement hiding.
          // unsigned DiagID = Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
          //                                         "Tag is hidden.");
          // Diags.Report(clang::SourceLocation(), DiagID);
          return false;
        }

        // If there is a described template, add that to the result instead
        // of the bare declaration.
        if (FoundDecl->declaresTemplate()) {
          if (auto *FD = dyn_cast<clang::FunctionDecl>(ND)) {
            clang::FunctionTemplateDecl *TempDecl
              = FD->getDescribedFunctionTemplate();
            if (TempDecl) 
              R.addDecl(TempDecl);
          } else if (auto *VD = dyn_cast<clang::VarDecl>(ND)){
            clang::VarTemplateDecl *TempDecl = VD->getDescribedVarTemplate();
            if(TempDecl)
              R.addDecl(TempDecl);
          }
        } else {
          R.addDecl(ND);
        }
      }
      break;
    }

    S = S->getParent();
  }
  return !R.empty();
}

clang::Sema::NameClassification Sema::classifyName(Scope* S,
      clang::IdentifierInfo *&Name, clang::SourceLocation NameLoc,
      clang::CorrectionCandidateCallback *CCC) {
        
  // Rewriting this so that it incorporates the early elaboration from
  // lookupUnqualifiedName.

  // Much of this code was take directly from clang::Sema::classifyName
  // the main difference here we have already parsed everything and are
  // elaborating an existing tree so knowing the next token's isn't really
  // all that useful.
  clang::DeclarationNameInfo NameInfo(Name, NameLoc);

  // We need to watch for constructor's within the current class.
  // TODO: Create a means to check if the current name is a class being elaborated?
  // Check if the current name is within scope and names a type.
  // if (SS.isSet() && CxxSema.isCurrentClassName(*Name, getCurClangScope(), &SS)) {
  //   // Per [class.qual]p2, this names the constructors of SS, not the
  //   // injected-class-name. We don't have a classification for that.
  //   // There's not much point caching this result, since the parser
  //   // will reject it later.
  //   return clang::Sema::NameClassification::Unknown();
  // }
  // clang::LookupResult &R
  clang::LookupResult Result(CxxSema, Name, NameLoc,
                             clang::Sema::LookupOrdinaryName);
  if(lookupUnqualifiedName(Result, S)) {
    // This may need to be expanded in the future?
    return clang::Sema::NameClassification::Error();
  }
//    LookupParsedName(Result, S, &SS, !CurMethod);
  bool IsFilteredTemplateName = true;
  switch (Result.getResultKind()) {
  case clang::LookupResult::NotFound:

//      // If an unqualified-id is followed by a '(', then we have a function
//      // call.
//      if (!SS.isSet() && NextToken.is(tok::l_paren)) {
//        // In C++, this is an ADL-only call.
//        // FIXME: Reference?
//        if (getLangOpts().CPlusPlus)
//          return NameClassification::UndeclaredNonType();
 
//        // C90 6.3.2.2:
//        //   If the expression that precedes the parenthesized argument list in a
//        //   function call consists solely of an identifier, and if no
//        //   declaration is visible for this identifier, the identifier is
//        //   implicitly declared exactly as if, in the innermost block containing
//        //   the function call, the declaration
//        //
//        //     extern int identifier ();
//        //
//        //   appeared.
//        //
//        // We also allow this in C99 as an extension.
//        if (NamedDecl *D = ImplicitlyDefineFunction(NameLoc, *Name, S))
//          return NameClassification::NonType(D);
//      }
 
//      if (getLangOpts().CPlusPlus2a && !SS.isSet() && NextToken.is(tok::less)) {
//        // In C++20 onwards, this could be an ADL-only call to a function
//        // template, and we're required to assume that this is a template name.
//        //
//        // FIXME: Find a way to still do typo correction in this case.
//        TemplateName Template =
//            Context.getAssumedTemplateName(NameInfo.getName());
//        return NameClassification::UndeclaredTemplate(Template);
//      }
 
//      // Perform typo correction to determine if there is another name that is
//      // close to this name.
//      if (!SecondTry && CCC) {
//        SecondTry = true;
//        if (TypoCorrection Corrected =
//                CorrectTypo(Result.getLookupNameInfo(), Result.getLookupKind(), S,
//                            &SS, *CCC, CTK_ErrorRecovery)) {
//          unsigned UnqualifiedDiag = diag::err_undeclared_var_use_suggest;
//          unsigned QualifiedDiag = diag::err_no_member_suggest;
 
//          NamedDecl *FirstDecl = Corrected.getFoundDecl();
//          NamedDecl *UnderlyingFirstDecl = Corrected.getCorrectionDecl();
//          if (getLangOpts().CPlusPlus && NextToken.is(tok::less) &&
//              UnderlyingFirstDecl && isa<TemplateDecl>(UnderlyingFirstDecl)) {
//            UnqualifiedDiag = diag::err_no_template_suggest;
//            QualifiedDiag = diag::err_no_member_template_suggest;
//          } else if (UnderlyingFirstDecl &&
//                     (isa<TypeDecl>(UnderlyingFirstDecl) ||
//                      isa<ObjCInterfaceDecl>(UnderlyingFirstDecl) ||
//                      isa<ObjCCompatibleAliasDecl>(UnderlyingFirstDecl))) {
//            UnqualifiedDiag = diag::err_unknown_typename_suggest;
//            QualifiedDiag = diag::err_unknown_nested_typename_suggest;
//          }
 
//          if (SS.isEmpty()) {
//            diagnoseTypo(Corrected, PDiag(UnqualifiedDiag) << Name);
//          } else {// FIXME: is this even reachable? Test it.
//            std::string CorrectedStr(Corrected.getAsString(getLangOpts()));
//            bool DroppedSpecifier = Corrected.WillReplaceSpecifier() &&
//                                    Name->getName().equals(CorrectedStr);
//            diagnoseTypo(Corrected, PDiag(QualifiedDiag)
//                                      << Name << computeDeclContext(SS, false)
//                                      << DroppedSpecifier << SS.getRange());
//          }
 
//          // Update the name, so that the caller has the new name.
//          Name = Corrected.getCorrectionAsIdentifierInfo();
 
//          // Typo correction corrected to a keyword.
//          if (Corrected.isKeyword())
//            return Name;
 
//          // Also update the LookupResult...
//          // FIXME: This should probably go away at some point
//          Result.clear();
//          Result.setLookupName(Corrected.getCorrection());
//          if (FirstDecl)
//            Result.addDecl(FirstDecl);
 
//          // If we found an Objective-C instance variable, let
//          // LookupInObjCMethod build the appropriate expression to
//          // reference the ivar.
//          // FIXME: This is a gross hack.
//          if (ObjCIvarDecl *Ivar = Result.getAsSingle<ObjCIvarDecl>()) {
//            DeclResult R =
//                LookupIvarInObjCMethod(Result, S, Ivar->getIdentifier());
//            if (R.isInvalid())
//              return NameClassification::Error();
//            if (R.isUsable())
//              return NameClassification::NonType(Ivar);
//          }
 
//          goto Corrected;
//        }
//      }
 
//      // We failed to correct; just fall through and let the parser deal with it.
//      Result.suppressDiagnostics();
    return clang::Sema::NameClassification::Unknown();
 
   case clang::LookupResult::NotFoundInCurrentInstantiation: {
     
//      // We performed name lookup into the current instantiation, and there were
//      // dependent bases, so we treat this result the same way as any other
//      // dependent nested-name-specifier.
 
//      // C++ [temp.res]p2:
//      //   A name used in a template declaration or definition and that is
//      //   dependent on a template-parameter is assumed not to name a type
//      //   unless the applicable name lookup finds a type name or the name is
//      //   qualified by the keyword typename.
//      //
//      // FIXME: If the next token is '<', we might want to ask the parser to
//      // perform some heroics to see if we actually have a
//      // template-argument-list, which would indicate a missing 'template'
//      // keyword here.
//      return NameClassification::DependentNonType();
//    }
    llvm_unreachable("The dependent non type not implemented yet.");
   }
 
  case clang::LookupResult::Found:
  case clang::LookupResult::FoundOverloaded:
  case clang::LookupResult::FoundUnresolvedValue:
    // If we found something then we are good and we need to continue, below.
    break;
 
  case clang::LookupResult::Ambiguous:
//      if (getLangOpts().CPlusPlus && NextToken.is(tok::less) &&
//          hasAnyAcceptableTemplateNames(Result, /*AllowFunctionTemplates=*/true,
//                                        /*AllowDependent=*/false)) {
//        // C++ [temp.local]p3:
//        //   A lookup that finds an injected-class-name (10.2) can result in an
//        //   ambiguity in certain cases (for example, if it is found in more than
//        //   one base class). If all of the injected-class-names that are found
//        //   refer to specializations of the same class template, and if the name
//        //   is followed by a template-argument-list, the reference refers to the
//        //   class template itself and not a specialization thereof, and is not
//        //   ambiguous.
//        //
//        // This filtering can make an ambiguous result into an unambiguous one,
//        // so try again after filtering out template names.
//        FilterAcceptableTemplateNames(Result);
       if (!Result.isAmbiguous()) {
         IsFilteredTemplateName = true;
         break;
       }
//      }
 
    // Diagnose the ambiguity and return an error.
    return clang::Sema::NameClassification::Error();
  }
  // TODO: Return valid templates from here when we have correctly identified
  // a valid thing we are looking for.
//    if (getLangOpts().CPlusPlus && NextToken.is(tok::less) &&
//        (IsFilteredTemplateName ||
//         hasAnyAcceptableTemplateNames(
//             Result, /*AllowFunctionTemplates=*/true,
//             /*AllowDependent=*/false,
//             /*AllowNonTemplateFunctions*/ !SS.isSet() &&
//                 getLangOpts().CPlusPlus2a))) {
//      // C++ [temp.names]p3:
//      //   After name lookup (3.4) finds that a name is a template-name or that
//      //   an operator-function-id or a literal- operator-id refers to a set of
//      //   overloaded functions any member of which is a function template if
//      //   this is followed by a <, the < is always taken as the delimiter of a
//      //   template-argument-list and never as the less-than operator.
//      // C++2a [temp.names]p2:
//      //   A name is also considered to refer to a template if it is an
//      //   unqualified-id followed by a < and name lookup finds either one
//      //   or more functions or finds nothing.
//      if (!IsFilteredTemplateName)
//        FilterAcceptableTemplateNames(Result);
 
  //   bool IsFunctionTemplate = false;
  //   bool IsVarTemplate = false;
  //   clang::TemplateName Template;
  //   if (Result.end() - Result.begin() > 1) {
  //     IsFunctionTemplate = true;
  //     Template = Context.CxxAST.getOverloadedTemplateName(Result.begin(),
  //                                                          Result.end());
  //   } else if (!Result.empty()) {
  //     auto *TD = cast<clang::TemplateDecl>(CxxSema.getAsTemplateNameDecl(
  //         *Result.begin(), /*AllowFunctionTemplates=*/true,
  //         /*AllowDependent=*/false));
  //     IsFunctionTemplate = isa<clang::FunctionTemplateDecl>(TD);
  //     IsVarTemplate = isa<clang::VarTemplateDecl>(TD);
 
  //     if (SS.isSet() && !SS.isInvalid())
  //       Template =
  //           Context.CxxAST.getQualifiedTemplateName(SS.getScopeRep(),
  //                                            /*TemplateKeyword=*/false, TD);
  //     else
  //       Template = clang::TemplateName(TD);
  //   } else {
  //     // All results were non-template functions. This is a function template
  //     // name.
  //     IsFunctionTemplate = true;
  //     Template = Context.CxxAST.getAssumedTemplateName(NameInfo.getName());
  //   }
 
  //   if (IsFunctionTemplate) {
  //     // Function templates always go through overload resolution, at which
  //     // point we'll perform the various checks (e.g., accessibility) we need
  //     // to based on which function we selected.
  //     Result.suppressDiagnostics();

  //     return clang::Sema::NameClassification::FunctionTemplate(Template);
  //   }
 
  //    return IsVarTemplate ? clang::Sema::NameClassification::VarTemplate(Template)
  //                         : clang::Sema::NameClassification::TypeTemplate(Template);
  // }
 
  // clang::NamedDecl *FirstDecl = (*Result.begin())->getUnderlyingDecl();
  // if (clang::TypeDecl *Type = dyn_cast<clang::TypeDecl>(FirstDecl)) {
  //   CxxSema.DiagnoseUseOfDecl(Type, NameLoc);
  //   CxxSema.MarkAnyDeclReferenced(Type->getLocation(), Type, /*OdrUse=*/false);
  //   clang::QualType T = Context.CxxAST.getTypeDeclType(Type);
  //   if (SS.isNotEmpty())
  //     return CxxSema.buildNestedType(*this, SS, T, NameLoc);
  //   return clang::ParsedType::make(T);
  // }
 
//    ObjCInterfaceDecl *Class = dyn_cast<ObjCInterfaceDecl>(FirstDecl);
//    if (!Class) {
//      // FIXME: It's unfortunate that we don't have a Type node for handling this.
//      if (ObjCCompatibleAliasDecl *Alias =
//              dyn_cast<ObjCCompatibleAliasDecl>(FirstDecl))
//        Class = Alias->getClassInterface();
//    }
 
//    if (Class) {
//      DiagnoseUseOfDecl(Class, NameLoc);
 
//      if (NextToken.is(tok::period)) {
//        // Interface. <something> is parsed as a property reference expression.
//        // Just return "unknown" as a fall-through for now.
//        Result.suppressDiagnostics();
//        return NameClassification::Unknown();
//      }
 
//      QualType T = Context.getObjCInterfaceType(Class);
//      return ParsedType::make(T);
//    }
 
//    // We can have a type template here if we're classifying a template argument.
//    if (isa<TemplateDecl>(FirstDecl) && !isa<FunctionTemplateDecl>(FirstDecl) &&
//        !isa<VarTemplateDecl>(FirstDecl))
//      return NameClassification::TypeTemplate(
//          TemplateName(cast<TemplateDecl>(FirstDecl)));
 
//    // Check for a tag type hidden by a non-type decl in a few cases where it
//    // seems likely a type is wanted instead of the non-type that was found.
//    bool NextIsOp = NextToken.isOneOf(tok::amp, tok::star);
//    if ((NextToken.is(tok::identifier) ||
//         (NextIsOp &&
//          FirstDecl->getUnderlyingDecl()->isFunctionOrFunctionTemplate())) &&
//        isTagTypeWithMissingTag(*this, Result, S, SS, Name, NameLoc)) {
//      TypeDecl *Type = Result.getAsSingle<TypeDecl>();
//      DiagnoseUseOfDecl(Type, NameLoc);
//      QualType T = Context.getTypeDeclType(Type);
//      if (SS.isNotEmpty())
//        return buildNestedType(*this, SS, T, NameLoc);
//      return ParsedType::make(T);
//    }
 
//    // FIXME: This is context-dependent. We need to defer building the member
//    // expression until the classification is consumed.
//    if (FirstDecl->isCXXClassMember())
//      return NameClassification::ContextIndependentExpr(
//          BuildPossibleImplicitMemberExpr(SS, SourceLocation(), Result, nullptr,
//                                          S));
 
//    // If we already know which single declaration is referenced, just annotate
//    // that declaration directly.
//    bool ADL = UseArgumentDependentLookup(SS, Result, NextToken.is(tok::l_paren));
//    if (Result.isSingleResult() && !ADL)
//      return NameClassification::NonType(Result.getRepresentativeDecl());
 
//    // Build an UnresolvedLookupExpr. Note that this doesn't depend on the
//    // context in which we performed classification, so it's safe to do now.
//    return NameClassification::ContextIndependentExpr(
//        BuildDeclarationNameExpr(SS, Result, ADL));
  
}
// Sema::NameClassification Sema::ClassifyName(Scope *S, CXXScopeSpec &SS,
//                                              IdentifierInfo *&Name,
//                                              SourceLocation NameLoc,
//                                              const Token &NextToken,
//                                              CorrectionCandidateCallback *CCC) {

//  }
 

clang::Scope *Sema::getCurClangScope() {
  return CxxSema.CurScope;
}

clang::Scope *Sema::enterClangScope(unsigned int ScopeFlags) {
  CxxSema.CurScope = new clang::Scope(getCurClangScope(), ScopeFlags, Diags);
  return CxxSema.CurScope;
}

clang::Scope *Sema::moveToParentScopeNoPop() {
  clang::Scope* S = CxxSema.CurScope;
  CxxSema.CurScope = CxxSema.CurScope->getParent();
  return S;
}

void Sema::ReEnterScope(clang::Scope* Scope) {
  assert(Scope && "Invalid scope.");
  assert(Scope->getParent() == CxxSema.CurScope &&
    "We are unable to re-enter this scope because we've already left the scope "
    "in which it was declared.");
  CxxSema.CurScope = Scope;
}

void Sema::leaveClangScope(clang::SourceLocation Loc) {
  assert(getCurClangScope() && "Clang scope imbalance!");

  // Inform the actions module that this scope is going away if there are any
  // decls in it.
  CxxSema.ActOnPopScope(Loc, getCurClangScope());

  clang::Scope *OldScope = getCurClangScope();
  CxxSema.CurScope = OldScope->getParent();

  delete OldScope;
}


} // namespace gold

