// RUN: %clang_cc1 -emit-llvm -fsemantic-interposition -pic-level 0 %s -o - | FileCheck %s -check-prefix=CHECK-NOPIC-NOPIE
// RUN: %clang_cc1 -emit-llvm -fsemantic-interposition -pic-level 1 %s -o - | FileCheck %s -check-prefix=CHECK-PIC-NOPIE
// RUN: %clang_cc1 -emit-llvm -fsemantic-interposition -pic-level 2 %s -o - | FileCheck %s -check-prefix=CHECK-PIC-NOPIE
// RUN: %clang_cc1 -emit-llvm -fsemantic-interposition -pic-level 0 %s -o - | FileCheck %s -check-prefix=CHECK-NOPIC-NOPIE
// RUN: %clang_cc1 -emit-llvm -fsemantic-interposition -pic-level 1 -pic-is-pie %s -o - | FileCheck %s -check-prefix=CHECK-PIC-PIE
// RUN: %clang_cc1 -emit-llvm -fsemantic-interposition -pic-level 2 -pic-is-pie %s -o - | FileCheck %s -check-prefix=CHECK-PIC-PIE

// CHECK-NOPIC-NOPIE-NOT: "SemanticInterposition"
// CHECK-PIC-NOPIE: !{{[0-9]+}} = !{i32 1, !"SemanticInterposition", i32 1}
// CHECK-PIC-PIE-NOT: "SemanticInterposition"
