; Test to make sure strbool attribute are crrectly upgraded
;
; RUN: llvm-as %s -o - | llvm-dis - | FileCheck %s


; CHECK-NOT: "no-nans-fp-math"="true"
; CHECK: "no-nans-fp-math"
; CHECK-NOT: "unsafe-fp-math"

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define float @foo() local_unnamed_addr #0 {
  ret float 1.000000e+00
}

attributes #0 = {"no-nans-fp-math"="true" "unsafe-fp-math"="false"}

