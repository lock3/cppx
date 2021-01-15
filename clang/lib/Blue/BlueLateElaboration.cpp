//=== BlueLateElaboration.cpp - Late elaboration for Gold Nodes ------------==//
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

#include "clang/Blue/BlueLateElaboration.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/Stmt.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/DiagnosticParse.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/ParsedAttr.h"
#include "clang/Sema/TypeLocUtil.h"
#include "clang/Sema/CXXFieldCollector.h"

#include "clang/Blue/BlueSema.h"
#include "clang/Blue/BlueElaborator.h"
#include "clang/AST/DeclCXX.h"

namespace blue {

LateElaboratedDecl::~LateElaboratedDecl() { }
void LateElaboratedDecl::ElaborateMethodDeclarations() { }
void LateElaboratedDecl::ElaborateMemberInitializers() { }
void LateElaboratedDecl::ElaborateDefaultParams() { }
void LateElaboratedDecl::ElaborateMethodDefs() { }
void LateElaboratedDecl::ElaborateAttributes() { }


LateElaboratedClass::LateElaboratedClass(Sema &S, ElaboratingClass *C)
  : SemaRef(S), Class(C) { }

LateElaboratedClass::~LateElaboratedClass() {
  SemaRef.deallocateElaboratingClass(Class);
}

void LateElaboratedClass::ElaborateMethodDeclarations() {
  Elaborator Elab(SemaRef);
  Elab.lateElaborateMethodDecls(*Class);
}

void LateElaboratedClass::ElaborateMemberInitializers() {
  Elaborator Elab(SemaRef);
  Elab.lateElaborateMemberInitializers(*Class);
}

void LateElaboratedClass::ElaborateMethodDefs() {
  Elaborator Elab(SemaRef);
  Elab.lateElaborateMethodDefs(*Class);
}

void LateElaboratedClass::ElaborateDefaultParams() {
  Elaborator Elab(SemaRef);
  Elab.lateElaborateDefaultParams(*Class);
}

void LateElaboratedClass::ElaborateAttributes() {
  Elaborator Elab(SemaRef);
  Elab.lateElaborateAttributes(*Class);
}

void LateElaboratedAttributeDecl::ElaborateAttributes() {
  Elaborator Elab(SemaRef);
  Elab.lateElaborateAttribute(*this);
}

void LateElaboratedMethodDef::ElaborateMethodDefs() {
  Elaborator Elab(SemaRef);
  Elab.lateElaborateMethodDef(*this);
}

void LateElaboratedMethodDeclaration::ElaborateMethodDeclarations() {
  Elaborator Elab(SemaRef);
  Elab.lateElaborateMethodDecl(*this);
}

void LateElaboratedMethodDeclaration::ElaborateDefaultParams() {
  Elaborator Elab(SemaRef);
  Elab.lateElaborateDefaultParams(*this);
}

void LateElaborateMemberInitializer::ElaborateMemberInitializers() {
  Elaborator Elab(SemaRef);
  Elab.lateElaborateMemberInitializer(*this);
}

ElaboratingClass::ElaboratingClass(
    Declaration *TagOrTemplate, bool TopLevelClass)
  : IsTopLevelClass(TopLevelClass),
    TemplateScope(TagOrTemplate->declaratorContainsClassTemplate()),
    TagOrTemplate(TagOrTemplate)
{ }

} // namespace blue