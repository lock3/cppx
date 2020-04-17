//===--- UninstantiatedTemplate.h - incomplete type base class -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file defines a type that's used to represent templates, groups of
//  templates, and groups of template functions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_TEMPLATE_TYPE_H
#define LLVM_CLANG_AST_TEMPLATE_TYPE_H

#include "clang/AST/Type.h"
#include "clang/AST/LocInfoType.h"
#include "llvm/ADT/APInt.h"

namespace clang {

class TemplateType : public Type, public llvm::FoldingSetNode {
  // bool IsTypeAlias = false;
  // This is optional.
  TemplateName Template;
protected:
  TemplateType(TypeClass TC, llvm::ArrayRef<QualType> const& Arguments);
public:
  unsigned getParameterCount() const {
    llvm_unreachable("Not implemented yet");
  }
  QualType getParameter(unsigned Index) const {
    llvm_unreachable("Not implemented yet");
  }

  /// I don't understand what this is, I understand what it's for but not what
  /// it's supposed to do.
  void Profile(llvm::FoldingSetNodeID &ID) {
    // Profile(ID, ParmType, PassingMode);
    llvm_unreachable("Working on it.");
  }
  using iterator = const QualType *;
  
  iterator begin() const { llvm_unreachable("Not implemented yet"); }
  iterator end() const { llvm_unreachable("Not implemented yet"); }
  
  TemplateName getTemplateName() const { llvm_unreachable("Not implemented yet"); }

  ArrayRef<QualType> template_parameters() const {
    // return {getArgs(), getNumArgs()};
    llvm_unreachable("Not implemented yet");
  }

  bool isSugared() const {
    // return !isDependentType() || isCurrentInstantiation() || isTypeAlias();
    return false;
  }

  QualType desugar() const {
    // return isTypeAlias() ? getAliasedType() : getCanonicalTypeInternal();
    return QualType();
  }

  // void Profile(llvm::FoldingSetNodeID &ID, const ASTContext &Ctx) {
  //   // Profile(ID, Template, template_arguments(), Ctx);
  //   // if (isTypeAlias())
  //   //   getAliasedType().Profile(ID);
  // }

  static void Profile(llvm::FoldingSetNodeID &ID, TemplateName T,
                      ArrayRef<TemplateArgument> Args,
                      const ASTContext &Context) { 
    llvm_unreachable("This hasn't been implemented yet.");
  }

  static bool classof(const Type *T) {
    return T->getTypeClass() == TemplateSpecialization;
  }
};
}

#endif