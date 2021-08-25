//===-- MipsTargetInfo.h - Mips Target Implementation -----------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MIPS_TARGETINFO_MIPSTARGETINFO_H
#define LLVM_LIB_TARGET_MIPS_TARGETINFO_MIPSTARGETINFO_H

#include "llvm/Support/Compiler.h"

namespace llvm LLVM_LIBRARY_VISIBILITY {

class Target;

Target &getTheMipsTarget();
Target &getTheMipselTarget();
Target &getTheMips64Target();
Target &getTheMips64elTarget();

} // namespace LLVM_LIBRARY_VISIBILITY

#endif // LLVM_LIB_TARGET_MIPS_TARGETINFO_MIPSTARGETINFO_H
