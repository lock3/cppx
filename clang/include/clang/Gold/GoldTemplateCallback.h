//===- GoldTemplateCallback.h ---------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  A template instantiation callback for Gold. Allows us to perform useful
//  actions on post-instantiated C++ declarations from the Gold elaborators.
//
//===----------------------------------------------------------------------===//

#include "clang/Sema/Sema.h"
#include "clang/Sema/TemplateInstCallback.h"

#include "clang/Gold/GoldSema.h"

#ifndef CLANG_GOLD_TEMPLATECALLBACK_H
#define CLANG_GOLD_TEMPLATECALLBACK_H

namespace gold {

using CodeSynthesisContext = clang::Sema::CodeSynthesisContext;

// A template instantiation callback object for Gold. This allows us to execute
// a callback function at the beginning/ending of each instantiation, such as
// adding instantiated UsingDecls to the Sema object or remapping
// gold::Declarations to their respective, instantiated clang::Decl *.
class GoldTemplateInstCallback : public clang::TemplateInstantiationCallback {
public:
  void initialize(const clang::Sema &S) override {}
  void finalize(const clang::Sema &S) override {}

  void initialize(Sema &S) override {
    SemaRef = &S;
    assert(SemaRef && "Sema object could not be created!");
  }
  void finalize(Sema &S) override {}

  void atTemplateBegin(const clang::Sema &TheSema,
                       const CodeSynthesisContext &Inst) override;

  void atTemplateEnd(const clang::Sema &TheSema,
                     const CodeSynthesisContext &Inst) override;


private:
  Sema *SemaRef = nullptr;
};

} // end namespace gold

#endif
