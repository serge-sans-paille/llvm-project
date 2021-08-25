//===- ARCTargetInfo.h - ARC Target Implementation ------------- *- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ARC_TARGETINFO_ARCTARGETINFO_H
#define LLVM_LIB_TARGET_ARC_TARGETINFO_ARCTARGETINFO_H

#include "llvm/Support/Compiler.h"

namespace llvm LLVM_LIBRARY_VISIBILITY {

class Target;

Target &getTheARCTarget();

} // namespace LLVM_LIBRARY_VISIBILITY

#endif // LLVM_LIB_TARGET_ARC_TARGETINFO_ARCTARGETINFO_H
