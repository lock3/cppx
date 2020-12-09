//===- GoldTemplateCallback.cpp -------------------------------------------===//
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

#include "clang/Gold/GoldTemplateCallback.h"

namespace gold {

void GoldTemplateInstCallback::atTemplateBegin(const clang::Sema &TheSema,
                                           const CodeSynthesisContext &Inst) { }

void GoldTemplateInstCallback::atTemplateEnd(const clang::Sema &TheSema,
                                           const CodeSynthesisContext &Inst) { }

} // end namespace gold