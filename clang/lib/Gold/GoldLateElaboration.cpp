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
void LateElaboratedDecl::ElaborateMethodDefs() { }
void LateElaboratedDecl::ElaborateFields() { }
void LateElaboratedDecl::ElaboratePragmas() { }


LateElaboratedClass::LateElaboratedClass(Sema &S, SyntaxContext &Ctxt,
      ElaboratingClass *C)
  : SemaRef(S), Context(Ctxt), Class(C) { }

LateElaboratedClass::~LateElaboratedClass() {

}

void LateElaboratedClass::ElaborateMethodDeclarations() {
  assert(false && "Not implemented yet");
}

void LateElaboratedClass::ElaborateMemberInitializers() {
  assert(false && "Not implemented yet");
}

void LateElaboratedClass::ElaborateMethodDefs() {
  assert(false && "Not implemented yet");
}

void LateElaboratedClass::ElaborateFields() {
  assert(false && "Not implemented yet");
}

void LateElaboratedClass::ElaboratePragmas() {
  assert(false && "Not implemented yet");
}

void LateElaboratedFieldDecl::ElaborateFields() {
  assert(false && "Not implemented yet");
}

void LateElaboratedMethodDef::ElaborateMethodDefs() {
  assert(false && "Not implemented yet");
}

void LateElaboratedMethodDeclaration::ElaborateMethodDeclarations() {
  assert(false && "Not implemented yet");
}

void LateElaborateMemberInitializer::ElaborateMemberInitializers() {
    assert(false && "Not implemented yet");
}



ElaboratingClass::ElaboratingClass(
    Declaration *TagOrTemplate, bool TopLevelClass)
  : IsTopLevelClass(TopLevelClass), TemplateScope(false),
    TagOrTemplate(TagOrTemplate)
{ }



// These are used in the following locations:
// Parser::ParseLexedMemberInitializers
// Parser::ParseLexedAttributes
// Parser::ParseLexedMethodDeclarations

}