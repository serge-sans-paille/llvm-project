; NOTE: Assertions have been autogenerated by utils/update_analyze_test_checks.py
; RUN: opt -mtriple=armv8.1m.main -mattr=+mve.fp -cost-model -analyze -cost-kind=throughput   < %s | FileCheck %s --check-prefix=THRU
; RUN: opt -mtriple=armv8.1m.main -mattr=+mve.fp -cost-model -analyze -cost-kind=latency      < %s | FileCheck %s --check-prefix=LATE
; RUN: opt -mtriple=armv8.1m.main -mattr=+mve.fp -cost-model -analyze -cost-kind=code-size    < %s | FileCheck %s --check-prefix=SIZE
; RUN: opt -mtriple=armv8.1m.main -mattr=+mve.fp -cost-model -analyze -cost-kind=size-latency < %s | FileCheck %s --check-prefix=SIZE_LATE

; Test a cross-section of intrinsics for various cost-kinds.
; Other test files may check for accuracy of a particular intrinsic
; across subtargets or types. This is just a sanity check using an
; ARM target and a legal scalar type (i32/float) and/or an
; illegal vector type (16 x i32/float).

declare i32 @llvm.smax.i32(i32, i32)
declare <16 x i32> @llvm.smax.v16i32(<16 x i32>, <16 x i32>)

declare float @llvm.fmuladd.f32(float, float, float)
declare <16 x float> @llvm.fmuladd.v16f32(<16 x float>, <16 x float>, <16 x float>)

declare i32 @llvm.cttz.i32(i32, i1)
declare <16 x i32> @llvm.cttz.v16i32(<16 x i32>, i1)

declare i32 @llvm.ctlz.i32(i32, i1)
declare <16 x i32> @llvm.ctlz.v16i32(<16 x i32>, i1)

declare i32 @llvm.fshl.i32(i32, i32, i32)
declare <16 x i32> @llvm.fshl.v16i32(<16 x i32>, <16 x i32>, <16 x i32>)

declare <16 x float> @llvm.masked.gather.v16f32.v16p0f32(<16 x float*>, i32, <16 x i1>, <16 x float>)
declare void @llvm.masked.scatter.v16f32.v16p0f32(<16 x float>, <16 x float*>, i32, <16 x i1>)
declare float @llvm.vector.reduce.fmax.v16f32(<16 x float>)

declare void @llvm.memcpy.p0i8.p0i8.i32(i8*, i8*, i32, i1)

define void @smax(i32 %a, i32 %b, <16 x i32> %va, <16 x i32> %vb) {
; THRU-LABEL: 'smax'
; THRU-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %s = call i32 @llvm.smax.i32(i32 %a, i32 %b)
; THRU-NEXT:  Cost Model: Found an estimated cost of 16 for instruction: %v = call <16 x i32> @llvm.smax.v16i32(<16 x i32> %va, <16 x i32> %vb)
; THRU-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: ret void
;
; LATE-LABEL: 'smax'
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %s = call i32 @llvm.smax.i32(i32 %a, i32 %b)
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v = call <16 x i32> @llvm.smax.v16i32(<16 x i32> %va, <16 x i32> %vb)
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE-LABEL: 'smax'
; SIZE-NEXT:  Cost Model: Found an estimated cost of 3 for instruction: %s = call i32 @llvm.smax.i32(i32 %a, i32 %b)
; SIZE-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %v = call <16 x i32> @llvm.smax.v16i32(<16 x i32> %va, <16 x i32> %vb)
; SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE_LATE-LABEL: 'smax'
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %s = call i32 @llvm.smax.i32(i32 %a, i32 %b)
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 4 for instruction: %v = call <16 x i32> @llvm.smax.v16i32(<16 x i32> %va, <16 x i32> %vb)
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
  %s = call i32 @llvm.smax.i32(i32 %a, i32 %b)
  %v = call <16 x i32> @llvm.smax.v16i32(<16 x i32> %va, <16 x i32> %vb)
  ret void
}

define void @fmuladd(float %a, float %b, float %c, <16 x float> %va, <16 x float> %vb, <16 x float> %vc) {
; THRU-LABEL: 'fmuladd'
; THRU-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %s = call float @llvm.fmuladd.f32(float %a, float %b, float %c)
; THRU-NEXT:  Cost Model: Found an estimated cost of 8 for instruction: %v = call <16 x float> @llvm.fmuladd.v16f32(<16 x float> %va, <16 x float> %vb, <16 x float> %vc)
; THRU-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: ret void
;
; LATE-LABEL: 'fmuladd'
; LATE-NEXT:  Cost Model: Found an estimated cost of 3 for instruction: %s = call float @llvm.fmuladd.f32(float %a, float %b, float %c)
; LATE-NEXT:  Cost Model: Found an estimated cost of 3 for instruction: %v = call <16 x float> @llvm.fmuladd.v16f32(<16 x float> %va, <16 x float> %vb, <16 x float> %vc)
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE-LABEL: 'fmuladd'
; SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %s = call float @llvm.fmuladd.f32(float %a, float %b, float %c)
; SIZE-NEXT:  Cost Model: Found an estimated cost of 8 for instruction: %v = call <16 x float> @llvm.fmuladd.v16f32(<16 x float> %va, <16 x float> %vb, <16 x float> %vc)
; SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE_LATE-LABEL: 'fmuladd'
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %s = call float @llvm.fmuladd.f32(float %a, float %b, float %c)
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 8 for instruction: %v = call <16 x float> @llvm.fmuladd.v16f32(<16 x float> %va, <16 x float> %vb, <16 x float> %vc)
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
  %s = call float @llvm.fmuladd.f32(float %a, float %b, float %c)
  %v = call <16 x float> @llvm.fmuladd.v16f32(<16 x float> %va, <16 x float> %vb, <16 x float> %vc)
  ret void
}

define void @cttz(i32 %a, <16 x i32> %va) {
; THRU-LABEL: 'cttz'
; THRU-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %s = call i32 @llvm.cttz.i32(i32 %a, i1 false)
; THRU-NEXT:  Cost Model: Found an estimated cost of 528 for instruction: %v = call <16 x i32> @llvm.cttz.v16i32(<16 x i32> %va, i1 false)
; THRU-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: ret void
;
; LATE-LABEL: 'cttz'
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %s = call i32 @llvm.cttz.i32(i32 %a, i1 false)
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v = call <16 x i32> @llvm.cttz.v16i32(<16 x i32> %va, i1 false)
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE-LABEL: 'cttz'
; SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %s = call i32 @llvm.cttz.i32(i32 %a, i1 false)
; SIZE-NEXT:  Cost Model: Found an estimated cost of 528 for instruction: %v = call <16 x i32> @llvm.cttz.v16i32(<16 x i32> %va, i1 false)
; SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE_LATE-LABEL: 'cttz'
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %s = call i32 @llvm.cttz.i32(i32 %a, i1 false)
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 528 for instruction: %v = call <16 x i32> @llvm.cttz.v16i32(<16 x i32> %va, i1 false)
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
  %s = call i32 @llvm.cttz.i32(i32 %a, i1 false)
  %v = call <16 x i32> @llvm.cttz.v16i32(<16 x i32> %va, i1 false)
  ret void
}

define void @ctlz(i32 %a, <16 x i32> %va) {
; THRU-LABEL: 'ctlz'
; THRU-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %s = call i32 @llvm.ctlz.i32(i32 %a, i1 true)
; THRU-NEXT:  Cost Model: Found an estimated cost of 528 for instruction: %v = call <16 x i32> @llvm.ctlz.v16i32(<16 x i32> %va, i1 true)
; THRU-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: ret void
;
; LATE-LABEL: 'ctlz'
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %s = call i32 @llvm.ctlz.i32(i32 %a, i1 true)
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v = call <16 x i32> @llvm.ctlz.v16i32(<16 x i32> %va, i1 true)
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE-LABEL: 'ctlz'
; SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %s = call i32 @llvm.ctlz.i32(i32 %a, i1 true)
; SIZE-NEXT:  Cost Model: Found an estimated cost of 528 for instruction: %v = call <16 x i32> @llvm.ctlz.v16i32(<16 x i32> %va, i1 true)
; SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE_LATE-LABEL: 'ctlz'
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %s = call i32 @llvm.ctlz.i32(i32 %a, i1 true)
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 528 for instruction: %v = call <16 x i32> @llvm.ctlz.v16i32(<16 x i32> %va, i1 true)
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
  %s = call i32 @llvm.ctlz.i32(i32 %a, i1 true)
  %v = call <16 x i32> @llvm.ctlz.v16i32(<16 x i32> %va, i1 true)
  ret void
}

define void @fshl(i32 %a, i32 %b, i32 %c, <16 x i32> %va, <16 x i32> %vb, <16 x i32> %vc) {
; THRU-LABEL: 'fshl'
; THRU-NEXT:  Cost Model: Found an estimated cost of 7 for instruction: %s = call i32 @llvm.fshl.i32(i32 %a, i32 %b, i32 %c)
; THRU-NEXT:  Cost Model: Found an estimated cost of 576 for instruction: %v = call <16 x i32> @llvm.fshl.v16i32(<16 x i32> %va, <16 x i32> %vb, <16 x i32> %vc)
; THRU-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: ret void
;
; LATE-LABEL: 'fshl'
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %s = call i32 @llvm.fshl.i32(i32 %a, i32 %b, i32 %c)
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v = call <16 x i32> @llvm.fshl.v16i32(<16 x i32> %va, <16 x i32> %vb, <16 x i32> %vc)
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE-LABEL: 'fshl'
; SIZE-NEXT:  Cost Model: Found an estimated cost of 8 for instruction: %s = call i32 @llvm.fshl.i32(i32 %a, i32 %b, i32 %c)
; SIZE-NEXT:  Cost Model: Found an estimated cost of 546 for instruction: %v = call <16 x i32> @llvm.fshl.v16i32(<16 x i32> %va, <16 x i32> %vb, <16 x i32> %vc)
; SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE_LATE-LABEL: 'fshl'
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 7 for instruction: %s = call i32 @llvm.fshl.i32(i32 %a, i32 %b, i32 %c)
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 564 for instruction: %v = call <16 x i32> @llvm.fshl.v16i32(<16 x i32> %va, <16 x i32> %vb, <16 x i32> %vc)
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
  %s = call i32 @llvm.fshl.i32(i32 %a, i32 %b, i32 %c)
  %v = call <16 x i32> @llvm.fshl.v16i32(<16 x i32> %va, <16 x i32> %vb, <16 x i32> %vc)
  ret void
}

define void @maskedgather(<16 x float*> %va, <16 x i1> %vb, <16 x float> %vc) {
; THRU-LABEL: 'maskedgather'
; THRU-NEXT:  Cost Model: Found an estimated cost of 576 for instruction: %v = call <16 x float> @llvm.masked.gather.v16f32.v16p0f32(<16 x float*> %va, i32 1, <16 x i1> %vb, <16 x float> %vc)
; THRU-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: ret void
;
; LATE-LABEL: 'maskedgather'
; LATE-NEXT:  Cost Model: Found an estimated cost of 3 for instruction: %v = call <16 x float> @llvm.masked.gather.v16f32.v16p0f32(<16 x float*> %va, i32 1, <16 x i1> %vb, <16 x float> %vc)
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE-LABEL: 'maskedgather'
; SIZE-NEXT:  Cost Model: Found an estimated cost of 576 for instruction: %v = call <16 x float> @llvm.masked.gather.v16f32.v16p0f32(<16 x float*> %va, i32 1, <16 x i1> %vb, <16 x float> %vc)
; SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE_LATE-LABEL: 'maskedgather'
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 576 for instruction: %v = call <16 x float> @llvm.masked.gather.v16f32.v16p0f32(<16 x float*> %va, i32 1, <16 x i1> %vb, <16 x float> %vc)
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
  %v = call <16 x float> @llvm.masked.gather.v16f32.v16p0f32(<16 x float*> %va, i32 1, <16 x i1> %vb, <16 x float> %vc)
  ret void
}

define void @maskedscatter(<16 x float> %va, <16 x float*> %vb, <16 x i1> %vc) {
; THRU-LABEL: 'maskedscatter'
; THRU-NEXT:  Cost Model: Found an estimated cost of 576 for instruction: call void @llvm.masked.scatter.v16f32.v16p0f32(<16 x float> %va, <16 x float*> %vb, i32 1, <16 x i1> %vc)
; THRU-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: ret void
;
; LATE-LABEL: 'maskedscatter'
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: call void @llvm.masked.scatter.v16f32.v16p0f32(<16 x float> %va, <16 x float*> %vb, i32 1, <16 x i1> %vc)
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE-LABEL: 'maskedscatter'
; SIZE-NEXT:  Cost Model: Found an estimated cost of 576 for instruction: call void @llvm.masked.scatter.v16f32.v16p0f32(<16 x float> %va, <16 x float*> %vb, i32 1, <16 x i1> %vc)
; SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE_LATE-LABEL: 'maskedscatter'
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 576 for instruction: call void @llvm.masked.scatter.v16f32.v16p0f32(<16 x float> %va, <16 x float*> %vb, i32 1, <16 x i1> %vc)
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
  call void @llvm.masked.scatter.v16f32.v16p0f32(<16 x float> %va, <16 x float*> %vb, i32 1, <16 x i1> %vc)
  ret void
}

define void @reduce_fmax(<16 x float> %va) {
; THRU-LABEL: 'reduce_fmax'
; THRU-NEXT:  Cost Model: Found an estimated cost of 632 for instruction: %v = call float @llvm.vector.reduce.fmax.v16f32(<16 x float> %va)
; THRU-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: ret void
;
; LATE-LABEL: 'reduce_fmax'
; LATE-NEXT:  Cost Model: Found an estimated cost of 3 for instruction: %v = call float @llvm.vector.reduce.fmax.v16f32(<16 x float> %va)
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE-LABEL: 'reduce_fmax'
; SIZE-NEXT:  Cost Model: Found an estimated cost of 620 for instruction: %v = call float @llvm.vector.reduce.fmax.v16f32(<16 x float> %va)
; SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE_LATE-LABEL: 'reduce_fmax'
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 628 for instruction: %v = call float @llvm.vector.reduce.fmax.v16f32(<16 x float> %va)
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
  %v = call float @llvm.vector.reduce.fmax.v16f32(<16 x float> %va)
  ret void
}

define void @memcpy(i8* %a, i8* %b, i32 %c) {
; THRU-LABEL: 'memcpy'
; THRU-NEXT:  Cost Model: Found an estimated cost of 4 for instruction: call void @llvm.memcpy.p0i8.p0i8.i32(i8* align 1 %a, i8* align 1 %b, i32 32, i1 false)
; THRU-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: ret void
;
; LATE-LABEL: 'memcpy'
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: call void @llvm.memcpy.p0i8.p0i8.i32(i8* align 1 %a, i8* align 1 %b, i32 32, i1 false)
; LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE-LABEL: 'memcpy'
; SIZE-NEXT:  Cost Model: Found an estimated cost of 4 for instruction: call void @llvm.memcpy.p0i8.p0i8.i32(i8* align 1 %a, i8* align 1 %b, i32 32, i1 false)
; SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; SIZE_LATE-LABEL: 'memcpy'
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 4 for instruction: call void @llvm.memcpy.p0i8.p0i8.i32(i8* align 1 %a, i8* align 1 %b, i32 32, i1 false)
; SIZE_LATE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
  call void @llvm.memcpy.p0i8.p0i8.i32(i8* align 1 %a, i8* align 1 %b, i32 32, i1 false)
  ret void
}
