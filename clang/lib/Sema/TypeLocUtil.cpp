//=== TypeLocUtil.cpp - Interface for Building TypeLocs -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2020, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements an interface for building TypeLocs used in dragon
//  language elaboration.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/TypeLoc.h"
#include "clang/Sema/TypeLocUtil.h"
#include "TypeLocBuilder.h"

namespace gold {

using clang::QualType;
using clang::TypeSourceInfo;
using clang::TypeLocBuilder;


template <typename TypeLocType>
static clang::TypeSourceInfo *BuildTypeLoc(clang::ASTContext &Context,
                                           clang::QualType Ty,
                                           SourceLocation Loc);

// Build a generic TypeSourceInfo for the specified type.
// Use the libsema TypeLocBuilder to create a TypeSourceInfo for a specific
// kind of TypeLoc.
// Needs to be explicitly instantiated for every TypeLocType.
// \param TLB is a TypeLocBuilder we want to carry through.
// \param Ty is the type we want to build a TypeSourceInfo out of.
// \param Loc is the SourceLocation of the TypeSourceInfo.
template <typename TypeLocType>
static TypeSourceInfo *BuildTypeLoc(clang::ASTContext &Context,
                             TypeLocBuilder &TLB, QualType Ty,
                             SourceLocation Loc) {
  auto TypeLocInstance = TLB.push<TypeLocType>(Ty);
  TypeLocInstance.setNameLoc(Loc);
  return TLB.getTypeSourceInfo(Context, Ty);
}

// Same as above, but uses a single-instance TypeLocBuilder.
template <typename TypeLocType>
TypeSourceInfo *BuildTypeLoc(clang::ASTContext &Context, QualType Ty,
                             SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<TypeLocType>(Context, TLB, Ty, Loc);
}

/// ======================================================================== ///
/// The following are explicit instantiations for every class of Type in     ///
/// libclang that don't fit into the generic case.                           ///
/// TODO: Find a way to remove instantiations we will never use
///       (objective-C stuff)                                                ///
/// TODO: BuildAnyType selects the instantation through a macro, but         ///
///       some TypeLocs (FunctionTypeLocs) have a different function         ///
///       signature. Is there a way to get around this?                      ///
/// ======================================================================== ///

template<> TypeSourceInfo *BuildTypeLoc<clang::BuiltinTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  auto TypeLocInstance = TLB.push<clang::BuiltinTypeLoc>(Ty);
  TypeLocInstance.setBuiltinLoc(Loc);
  return TLB.getTypeSourceInfo(Ctx, Ty);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::BuiltinTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::BuiltinTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::PointerTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  QualType InnerType = Ty->getAs<clang::PointerType>()->getPointeeType();
  BuildAnyTypeLoc(Ctx, TLB, InnerType, Loc);

  auto TypeLocInstance = TLB.push<clang::PointerTypeLoc>(Ty);
  TypeLocInstance.setStarLoc(Loc);
  return TLB.getTypeSourceInfo(Ctx, Ty);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::PointerTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::PointerTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::BlockPointerTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::BlockPointerTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::BlockPointerTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ReferenceTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ReferenceTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::ReferenceTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::LValueReferenceTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::LValueReferenceTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::LValueReferenceTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::RValueReferenceTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::RValueReferenceTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::RValueReferenceTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::MemberPointerTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::MemberPointerTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::MemberPointerTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ArrayTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ArrayTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::ArrayTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ConstantArrayTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ConstantArrayTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::ConstantArrayTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::IncompleteArrayTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::IncompleteArrayTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::IncompleteArrayTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::VariableArrayTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::VariableArrayTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::VariableArrayTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DependentSizedArrayTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DependentSizedArrayTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::DependentSizedArrayTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DependentSizedExtVectorTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DependentSizedExtVectorTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::DependentSizedExtVectorTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DependentAddressSpaceTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DependentAddressSpaceTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::DependentAddressSpaceTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::VectorTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::VectorTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::VectorTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DependentVectorTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DependentVectorTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::DependentVectorTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ExtVectorTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ExtVectorTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::ExtVectorTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::FunctionTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("Use BuildFunctionTypeLoc.");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::FunctionTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::FunctionTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::FunctionProtoTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("Use BuildFunctionTypeLoc.");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::FunctionProtoTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::FunctionProtoTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::FunctionNoProtoTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("Use BuildFunctionTypeLoc.");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::FunctionNoProtoTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::FunctionNoProtoTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::UnresolvedUsingTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::UnresolvedUsingTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::UnresolvedUsingTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ParenTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ParenTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::ParenTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::TypedefTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::TypedefTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::TypedefTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::MacroQualifiedTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::MacroQualifiedTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::MacroQualifiedTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::AdjustedTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::AdjustedTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::AdjustedTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DecayedTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DecayedTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::DecayedTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::TypeOfExprTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::TypeOfExprTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::TypeOfExprTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::TypeOfTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::TypeOfTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::TypeOfTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DecltypeTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DecltypeTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::DecltypeTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ReflectedTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ReflectedTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::ReflectedTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::UnaryTransformTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::UnaryTransformTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::UnaryTransformTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::TagTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::TagTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::TagTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::RecordTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::RecordTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::RecordTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::EnumTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::EnumTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::EnumTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ElaboratedTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ElaboratedTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::ElaboratedTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::AttributedTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::AttributedTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::AttributedTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::TemplateTypeParmTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::TemplateTypeParmTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::TemplateTypeParmTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::SubstTemplateTypeParmTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::SubstTemplateTypeParmTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::SubstTemplateTypeParmTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::SubstTemplateTypeParmPackTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::SubstTemplateTypeParmPackTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::SubstTemplateTypeParmPackTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::TemplateSpecializationTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::TemplateSpecializationTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::TemplateSpecializationTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DeducedTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DeducedTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::DeducedTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DeducedTemplateSpecializationTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DeducedTemplateSpecializationTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::DeducedTemplateSpecializationTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::InjectedClassNameTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::InjectedClassNameTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::InjectedClassNameTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DependentNameTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DependentNameTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::DependentNameTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DependentTemplateSpecializationTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::DependentTemplateSpecializationTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::DependentTemplateSpecializationTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::PackExpansionTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::PackExpansionTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::PackExpansionTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::CXXDependentVariadicReifierTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::CXXDependentVariadicReifierTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::CXXDependentVariadicReifierTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::CXXRequiredTypeTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::CXXRequiredTypeTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::CXXRequiredTypeTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ObjCObjectTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ObjCObjectTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::ObjCObjectTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ObjCObjectPointerTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::ObjCObjectPointerTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::ObjCObjectPointerTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::PipeTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::PipeTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::PipeTypeLoc>(Context, TLB, Ty, Loc);
}

template<> TypeSourceInfo *BuildTypeLoc<clang::AtomicTypeLoc>
(clang::ASTContext &Ctx, TypeLocBuilder &TLB, QualType Ty, SourceLocation Loc) {
  llvm_unreachable("unimplemented");
}

template<> TypeSourceInfo *BuildTypeLoc<clang::AtomicTypeLoc>
(clang::ASTContext &Context, QualType Ty, SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildTypeLoc<clang::AtomicTypeLoc>(Context, TLB, Ty, Loc);
}

TypeSourceInfo *BuildAnyTypeLoc(clang::ASTContext &Context,
                                TypeLocBuilder &TLB, QualType T,
                                SourceLocation Loc) {
  switch (T->getTypeClass()) {
#define ABSTRACT_TYPE(CLASS, PARENT)
#define TYPE(CLASS, PARENT)                                   \
  case clang::Type::CLASS:                                    \
    return BuildTypeLoc<clang::CLASS##TypeLoc>(Context, TLB, T, Loc);
#include "clang/AST/TypeNodes.inc"
  }
}

TypeSourceInfo *BuildAnyTypeLoc(clang::ASTContext &Context, QualType T,
                                SourceLocation Loc) {
  TypeLocBuilder TLB;
  return BuildAnyTypeLoc(Context, TLB, T, Loc);
}

TypeSourceInfo *BuildFunctionTypeLoc(clang::ASTContext &Context, QualType Ty,
    SourceLocation BeginLoc, SourceLocation LParenLoc,
    SourceLocation RParenLoc, clang::SourceRange ExceptionSpecRange,
    SourceLocation EndLoc, llvm::SmallVectorImpl<clang::ParmVarDecl *> &Params) {
  assert(Ty->isFunctionType() &&
         "Constructing FunctionTypeLoc out of non-function type");

  TypeLocBuilder TLB;

  // Push the return type to the TypeLocBuilder.
  QualType ReturnType = Ty->getAs<clang::FunctionType>()->getReturnType();
  BuildAnyTypeLoc(Context, TLB, ReturnType, BeginLoc);

  clang::FunctionTypeLoc NewTL = TLB.push<clang::FunctionTypeLoc>(Ty);
  NewTL.setLocalRangeBegin(BeginLoc);
  NewTL.setLParenLoc(LParenLoc);
  NewTL.setRParenLoc(RParenLoc);
  NewTL.setExceptionSpecRange(ExceptionSpecRange);
  NewTL.setLocalRangeEnd(EndLoc);

  for (unsigned I = 0; I < Params.size(); ++I)
    NewTL.setParam(I, Params[I]);

  return TLB.getTypeSourceInfo(Context, Ty);
}

} // namespace gold
