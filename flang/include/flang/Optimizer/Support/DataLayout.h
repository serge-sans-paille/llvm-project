//===-- Optimizer/Support/DataLayout.h --------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Coding style: https://mlir.llvm.org/getting_started/DeveloperGuide/
//
//===----------------------------------------------------------------------===//

#ifndef FORTRAN_OPTIMIZER_SUPPORT_DATALAYOUT_H
#define FORTRAN_OPTIMIZER_SUPPORT_DATALAYOUT_H

#include "mlir/Interfaces/DataLayoutInterfaces.h"
#include <optional>

namespace mlir {
class ModuleOp;
}
namespace llvm {
class DataLayout;
}

constexpr size_t FLANG_TARGET_SIZEOF_UINT = 4;
constexpr size_t FLANG_TARGET_SIZEOF_INT = 4;
constexpr size_t FLANG_TARGET_SIZEOF_SHORT = 2;
constexpr size_t FLANG_TARGET_SIZEOF_CHAR = 1;
constexpr size_t FLANG_TARGET_SIZEOF_SCHAR = 1;
constexpr size_t FLANG_TARGET_SIZEOF_UCHAR = 1;
constexpr size_t FLANG_TARGET_SIZEOF_CHAR16 = 2;
constexpr size_t FLANG_TARGET_SIZEOF_CHAR32 = 4;
constexpr size_t FLANG_TARGET_SIZEOF_LONG = 4;
constexpr size_t FLANG_TARGET_SIZEOF_LONGLONG = 8;
constexpr size_t FLANG_TARGET_SIZEOF_ULONG = 4;
constexpr size_t FLANG_TARGET_SIZEOF_ULONGLONG = 8;
constexpr size_t FLANG_TARGET_SIZEOF_SIZE_T = 4;
constexpr size_t FLANG_TARGET_SIZEOF_FLOAT = 4;
constexpr size_t FLANG_TARGET_SIZEOF_DOUBLE = 8;

namespace fir::support {
/// Create an mlir::DataLayoutSpecInterface attribute from an llvm::DataLayout
/// and set it on the provided mlir::ModuleOp.
/// Also set the llvm.data_layout attribute with the string representation of
/// the llvm::DataLayout on the module.
/// These attributes are replaced if they were already set.
void setMLIRDataLayout(mlir::ModuleOp mlirModule, const llvm::DataLayout &dl);

/// Create an mlir::DataLayoutSpecInterface from the llvm.data_layout attribute
/// if one is provided. If such attribute is not available, create a default
/// target independent layout when allowDefaultLayout is true. Otherwise do
/// nothing.
void setMLIRDataLayoutFromAttributes(mlir::ModuleOp mlirModule,
                                     bool allowDefaultLayout);

/// Create mlir::DataLayout from the data layout information on the
/// mlir::Module. Creates the data layout information attributes with
/// setMLIRDataLayoutFromAttributes if the DLTI attribute is not yet set. If no
/// information is present at all and \p allowDefaultLayout is false, returns
/// std::nullopt.
std::optional<mlir::DataLayout>
getOrSetDataLayout(mlir::ModuleOp mlirModule, bool allowDefaultLayout = false);

} // namespace fir::support

#endif // FORTRAN_OPTIMIZER_SUPPORT_DATALAYOUT_H
