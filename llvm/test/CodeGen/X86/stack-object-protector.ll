; RUN: llc -mtriple=x86_64-pc-linux-gnu --stop-after=stack-object-canary < %s -o - | FileCheck %s

%struct.bar.0 = type { i32, i32 }
%struct.bar.1 = type { i32*, i32 }
%struct.foo.0 = type { [16 x i8], i32}
%struct.foo.1 = type { %struct.foo.0, i8 }
%struct.foo.2 = type { [2 x %struct.bar.1], i8 }


declare i8 @foo(i8*);

; test0a: no sspobject attribute => no protector
; CHECK-LABEL: define i8 @test0a(i32 %n) {
; CHECK-NOT: %StackGuard
define i8 @test0a(i32 %n) {
entry:
  %buf = alloca [16 x i8]
  %bufview = getelementptr inbounds [16 x i8], [16 x i8]* %buf, i32 0, i32 %n
  store i8 1, i8* %bufview
  %retval = load i8, i8* %bufview
  ret i8 %retval
}

; test0b: sspobject attribute => protector
; CHECK-LABEL:  define i8 @test0b(i32 %n) #0 {

; CHECK-LABEL:  entry:
; CHECK-NEXT:    %StackGuard = load volatile i8*, i8* addrspace(257)* inttoptr (i32 40 to i8* addrspace(257)*)
; CHECK-NEXT:    %0 = ptrtoint i8* %StackGuard to i32
; CHECK-NEXT:    %1 = alloca %stack_object_canary_ty
; CHECK-NEXT:    %2 = getelementptr inbounds %stack_object_canary_ty, %stack_object_canary_ty* %1, i32 0, i32 0
; CHECK-NEXT:    %3 = getelementptr inbounds %stack_object_canary_ty, %stack_object_canary_ty* %1, i32 0, i32 1
; CHECK-NEXT:    store volatile i32 %0, i32* %3
; CHECK-NEXT:    %n.c = sext i32 %n to i64
; CHECK-NEXT:    %bufview.offs = add i64 %n.c, 0
; CHECK-NEXT:    %4 = add i64 0, %bufview.offs
; CHECK-NEXT:    %bufview = getelementptr inbounds [16 x i8], [16 x i8]* %2, i32 0, i32 %n
; CHECK-NEXT:    store i8 1, i8* %bufview
; CHECK-NEXT:    %5 = getelementptr inbounds %stack_object_canary_ty, %stack_object_canary_ty* %1, i32 0, i32 1
; CHECK-NEXT:    %6 = load volatile i32, i32* %5
; CHECK-NEXT:    %canary_check = xor i32 %6, %0
; CHECK-NEXT:    %7 = icmp eq i32 %canary_check, 0
; CHECK-NEXT:    br i1 %7, label %8, label %stack_check_exit_node

; CHECK-LABEL:  8:
; CHECK-NEXT:    %retval = load i8, i8* %bufview
; CHECK-NEXT:    ret i8 %retval

; CHECK-LABEL:  stack_check_exit_node:
; CHECK-NEXT:    call void @llvm.trap()
; CHECK-NEXT:    unreachable
; CHECK-NEXT:  }

define i8 @test0b(i32 %n) #0 {
entry:
  %buf = alloca [16 x i8]
  %bufview = getelementptr inbounds [16 x i8], [16 x i8]* %buf, i32 0, i32 %n
  store i8 1, i8* %bufview
  %retval = load i8, i8* %bufview
  ret i8 %retval
}

; test1a: out of bound access => protector
; CHECK-LABEL:  define i8 @test1a(i1 %cond, i32 %n) #0 {

; CHECK-LABEL:  entry:
; CHECK-NEXT:    %StackGuard = load volatile i8*, i8* addrspace(257)* inttoptr (i32 40 to i8* addrspace(257)*)
; CHECK-NEXT:    %0 = ptrtoint i8* %StackGuard to i32
; CHECK-NEXT:    %1 = alloca %stack_object_canary_ty.0
; CHECK-NEXT:    %2 = getelementptr inbounds %stack_object_canary_ty.0, %stack_object_canary_ty.0* %1, i32 0, i32 0
; CHECK-NEXT:    %3 = getelementptr inbounds %stack_object_canary_ty.0, %stack_object_canary_ty.0* %1, i32 0, i32 1
; CHECK-NEXT:    store volatile i32 %0, i32* %3
; CHECK-NEXT:    %index = select i1 %cond, i32 %n, i32 12
; CHECK-NEXT:    %index.c = sext i32 %index to i64
; CHECK-NEXT:    %bufview.offs = add i64 %index.c, 0
; CHECK-NEXT:    %4 = add i64 0, %bufview.offs
; CHECK-NEXT:    %bufview = getelementptr inbounds [16 x i8], [16 x i8]* %2, i32 0, i32 %index
; CHECK-NEXT:    store i8 1, i8* %bufview
; CHECK-NEXT:    %5 = getelementptr inbounds %stack_object_canary_ty.0, %stack_object_canary_ty.0* %1, i32 0, i32 1
; CHECK-NEXT:    %6 = load volatile i32, i32* %5
; CHECK-NEXT:    %canary_check = xor i32 %6, %0
; CHECK-NEXT:    %7 = icmp eq i32 %canary_check, 0
; CHECK-NEXT:    br i1 %7, label %8, label %stack_check_exit_node

; CHECK-LABEL:  8:
; CHECK-NEXT:    %retval = load i8, i8* %bufview
; CHECK-NEXT:    ret i8 %retval

; CHECK-LABEL:  stack_check_exit_node:
; CHECK-NEXT:    call void @llvm.trap()
; CHECK-NEXT:    unreachable
; CHECK:  }

define i8 @test1a(i1 %cond, i32 %n) #0 {
entry:
  %buf = alloca [16 x i8]
  %index = select i1 %cond, i32 %n, i32 12
  %bufview = getelementptr inbounds [16 x i8], [16 x i8]* %buf, i32 0, i32 %index
  store i8 1, i8* %bufview
  %retval = load i8, i8* %bufview
  ret i8 %retval
}

; test1b: inbound access => no protector
; CHECK-LABEL:  define i8 @test1b(i1 %cond, i32 %n) #0 {
; CHECK-NOT: %StackGuard
define i8 @test1b(i1 %cond, i32 %n) #0 {
entry:
  %buf = alloca [16 x i8]
  %index = select i1 %cond, i32 8, i32 12
  %bufview = getelementptr inbounds [16 x i8], [16 x i8]* %buf, i32 0, i32 %index
  store i8 1, i8* %bufview
  %retval = load i8, i8* %bufview
  ret i8 %retval
}

; test2a: bound loop access => one protector outside of the loop
; CHECK-LABEL:  define i8 @test2a(i32 %n) #0 {

; CHECK-LABEL:  entry:
; CHECK-NEXT:    %StackGuard = load volatile i8*, i8* addrspace(257)* inttoptr (i32 40 to i8* addrspace(257)*)
; CHECK-NEXT:    %0 = ptrtoint i8* %StackGuard to i32
; CHECK-NEXT:    %1 = alloca %stack_object_canary_ty.1
; CHECK-NEXT:    %2 = getelementptr inbounds %stack_object_canary_ty.1, %stack_object_canary_ty.1* %1, i32 0, i32 0
; CHECK-NEXT:    %3 = getelementptr inbounds %stack_object_canary_ty.1, %stack_object_canary_ty.1* %1, i32 0, i32 1
; CHECK-NEXT:    store volatile i32 %0, i32* %3
; CHECK-NEXT:    br label %loop

; CHECK-LABEL:  loop:
; CHECK-NEXT:    %lsr.iv1 = phi [10 x i8]* [ %4, %loop ], [ %2, %entry ]
; CHECK-NEXT:    %lsr.iv = phi i32 [ %lsr.iv.next, %loop ], [ %n, %entry ]
; CHECK-NEXT:    %lsr.iv12 = bitcast [10 x i8]* %lsr.iv1 to i8*
; CHECK-NEXT:    store i8 12, i8* %lsr.iv12
; CHECK-NEXT:    %lsr.iv.next = add i32 %lsr.iv, -1
; CHECK-NEXT:    %scevgep = getelementptr [10 x i8], [10 x i8]* %lsr.iv1, i64 0, i64 1
; CHECK-NEXT:    %4 = bitcast i8* %scevgep to [10 x i8]*
; CHECK-NEXT:    %cond = icmp eq i32 %lsr.iv.next, 0
; CHECK-NEXT:    br i1 %cond, label %exit, label %loop

; CHECK-LABEL:  exit:
; CHECK-NEXT:    %other_view3 = bitcast [10 x i8]* %2 to i8*
; CHECK-NEXT:    %5 = getelementptr inbounds %stack_object_canary_ty.1, %stack_object_canary_ty.1* %1, i32 0, i32 1
; CHECK-NEXT:    %6 = load volatile i32, i32* %5
; CHECK-NEXT:    %canary_check = xor i32 %6, %0
; CHECK-NEXT:    %7 = icmp eq i32 %canary_check, 0
; CHECK-NEXT:    br i1 %7, label %8, label %stack_check_exit_node

; CHECK-LABEL:  8:
; CHECK-NEXT:    %res = call i8 @foo(i8* %other_view3)
; CHECK-NEXT:    ret i8 %res

; CHECK-LABEL:  stack_check_exit_node:
; CHECK-NEXT:    call void @llvm.trap()
; CHECK-NEXT:    unreachable
; CHECK-NEXT:  }


define i8 @test2a(i32 %n) #0 {
entry:
  %buf = alloca [10 x i8]
  br label %loop

loop:
  %loop_index = phi i32 [ 0, %entry ], [ %inc, %loop ]
  %view = getelementptr inbounds [10 x i8], [10 x i8]* %buf, i32 0, i32 %loop_index
  store i8 12, i8* %view
  %inc = add nuw nsw i32 %loop_index, 1
  %cond = icmp eq i32 %inc, %n
  br i1 %cond, label %exit, label %loop

exit:
  %other_view = getelementptr inbounds [10 x i8], [10 x i8]* %buf, i32 0, i32 0
  %res = call i8 @foo(i8* %other_view)
  ret i8 %res
}

; test2b: inbound loop access => no protector
; CHECK-LABEL:  define i8 @test2b(i32 %n) #0 {
; CHECK-NOT: %StackGuard
define i8 @test2b(i32 %n) #0 {
entry:
  %buf = alloca [10 x i8]
  br label %loop

loop:
  %loop_index = phi i32 [ 0, %entry ], [ %inc, %loop ]
  %view = getelementptr inbounds [10 x i8], [10 x i8]* %buf, i32 0, i32 %loop_index
  store i8 12, i8* %view
  %inc = add nuw nsw i32 %loop_index, 1
  %cond = icmp eq i32 %inc, 10
  br i1 %cond, label %exit, label %loop

exit:
  %other_view = getelementptr inbounds [10 x i8], [10 x i8]* %buf, i32 0, i32 0
  %res = call i8 @foo(i8* %other_view)
  ret i8 %res
}


; test2c: inbound strided loop access => no protector
; CHECK-LABEL:  define i32 @test2c(i32 %s) #1 {
; CHECK-NOT: %StackGuard
define i32 @test2c(i32 %s) #1 {
entry:
  %g = alloca [200 x i32], align 16
  %0 = bitcast [200 x i32]* %g to i8*
  %mul = mul nsw i32 %s, %s
  br label %for.body

for.cond.cleanup:
  %idxprom2 = sext i32 %s to i64
  %arrayidx3 = getelementptr inbounds [200 x i32], [200 x i32]* %g, i64 0, i64 %idxprom2
  %1 = load i32, i32* %arrayidx3, align 4
  ret i32 %1

for.body:
  %indvars.iv = phi i64 [ 0, %entry ], [ %indvars.iv.next, %for.body ]
  %2 = shl nuw nsw i64 %indvars.iv, 1
  %3 = or i64 %2, 1
  %arrayidx = getelementptr inbounds [200 x i32], [200 x i32]* %g, i64 0, i64 %3
  store i32 %mul, i32* %arrayidx, align 4
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %exitcond = icmp eq i64 %indvars.iv.next, 50
  br i1 %exitcond, label %for.cond.cleanup, label %for.body
}

attributes #0 = { sspobject}
attributes #1 = { sspobject "target-cpu"="x86-64"}
