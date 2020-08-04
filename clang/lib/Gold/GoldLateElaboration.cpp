//=== GoldLateElaboration.cpp - Late elaboration for Gold Nodes ------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file contains the definitions for the late LateElaboratedDecl classes
//  and any associated declarations.
//
//===----------------------------------------------------------------------===//

#include "clang/Gold/GoldLateElaboration.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/Stmt.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/DiagnosticParse.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/ParsedAttr.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/Sema/TypeLocUtil.h"
#include "clang/Sema/CXXFieldCollector.h"

#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldStmtElaborator.h"
#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/AST/DeclCXX.h"

namespace gold {

LateElaboratedDecl::~LateElaboratedDecl() { }
void LateElaboratedDecl::ElaborateMethodDeclarations() { }
void LateElaboratedDecl::ElaborateMemberInitializers() { }
void LateElaboratedDecl::ElaborateDefaultParams() { }
void LateElaboratedDecl::ElaborateMethodDefs() { }
void LateElaboratedDecl::ElaborateAttributes() { }


LateElaboratedClass::LateElaboratedClass(Sema &S, SyntaxContext &Ctxt,
      ElaboratingClass *C)
  : SemaRef(S), Context(Ctxt), Class(C) { }

LateElaboratedClass::~LateElaboratedClass() {
  SemaRef.deallocateElaboratingClass(Class);
}

void LateElaboratedClass::ElaborateMethodDeclarations() {
  Elaborator Elab(Context, SemaRef);
  Elab.lateElaborateMethodDecls(*Class);
}

void LateElaboratedClass::ElaborateMemberInitializers() {
  Elaborator Elab(Context, SemaRef);
  Elab.lateElaborateMemberInitializers(*Class);
}

void LateElaboratedClass::ElaborateMethodDefs() {
  Elaborator Elab(Context, SemaRef);
  Elab.lateElaborateMethodDefs(*Class);
}

void LateElaboratedClass::ElaborateDefaultParams() {
  Elaborator Elab(Context, SemaRef);
  Elab.lateElaborateDefaultParams(*Class);
}

void LateElaboratedClass::ElaborateAttributes() {
  Elaborator Elab(Context, SemaRef);
  Elab.lateElaborateAttributes(*Class);
}

void LateElaboratedAttributeDecl::ElaborateAttributes() {
  Elaborator Elab(Context, SemaRef);
  Elab.lateElaborateAttribute(*this);
}

void LateElaboratedMethodDef::ElaborateMethodDefs() {
  Elaborator Elab(Context, SemaRef);
  Elab.lateElaborateMethodDef(*this);
}

void LateElaboratedMethodDeclaration::ElaborateMethodDeclarations() {
  Elaborator Elab(Context, SemaRef);
  Elab.lateElaborateMethodDecl(*this);
}

void LateElaboratedMethodDeclaration::ElaborateDefaultParams() {
  Elaborator Elab(Context, SemaRef);
  Elab.lateElaborateDefaultParams(*this);
}

void LateElaborateMemberInitializer::ElaborateMemberInitializers() {
  Elaborator Elab(Context, SemaRef);
  Elab.lateElaborateMemberInitializer(*this);
}

ElaboratingClass::ElaboratingClass(
    Declaration *TagOrTemplate, bool TopLevelClass)
  : IsTopLevelClass(TopLevelClass),
    TemplateScope(TagOrTemplate->declaresTemplateType()),
    TagOrTemplate(TagOrTemplate)
{ }

} // namespace gold