; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt -S -early-cse < %s | FileCheck %s --check-prefixes=CHECK,NO_ASSUME
; RUN: opt -S -basicaa -early-cse-memssa < %s | FileCheck %s --check-prefixes=CHECK,NO_ASSUME
; RUN: opt -S -basicaa -early-cse-memssa --enable-knowledge-retention < %s | FileCheck %s --check-prefixes=CHECK,USE_ASSUME

declare void @clobber_and_use(i32)

define void @f_0(i32* %ptr) {
; NO_ASSUME-LABEL: @f_0(
; NO_ASSUME-NEXT:    [[VAL0:%.*]] = load i32, i32* [[PTR:%.*]], !invariant.load !0
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; NO_ASSUME-NEXT:    ret void
;
; USE_ASSUME-LABEL: @f_0(
; USE_ASSUME-NEXT:    [[VAL0:%.*]] = load i32, i32* [[PTR:%.*]], !invariant.load !0
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; USE_ASSUME-NEXT:    call void @llvm.assume(i1 true) [ "dereferenceable"(i32* [[PTR]], i64 4), "nonnull"(i32* [[PTR]]) ]
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; USE_ASSUME-NEXT:    call void @llvm.assume(i1 true) [ "dereferenceable"(i32* [[PTR]], i64 4), "nonnull"(i32* [[PTR]]) ]
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; USE_ASSUME-NEXT:    ret void
;

  %val0 = load i32, i32* %ptr, !invariant.load !{}
  call void @clobber_and_use(i32 %val0)
  %val1 = load i32, i32* %ptr, !invariant.load !{}
  call void @clobber_and_use(i32 %val1)
  %val2 = load i32, i32* %ptr, !invariant.load !{}
  call void @clobber_and_use(i32 %val2)
  ret void
}

define void @f_1(i32* %ptr) {
; We can forward invariant loads to non-invariant loads.
; NO_ASSUME-LABEL: @f_1(
; NO_ASSUME-NEXT:    [[VAL0:%.*]] = load i32, i32* [[PTR:%.*]], !invariant.load !0
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; NO_ASSUME-NEXT:    ret void
;
; USE_ASSUME-LABEL: @f_1(
; USE_ASSUME-NEXT:    [[VAL0:%.*]] = load i32, i32* [[PTR:%.*]], !invariant.load !0
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; USE_ASSUME-NEXT:    call void @llvm.assume(i1 true) [ "dereferenceable"(i32* [[PTR]], i64 4), "nonnull"(i32* [[PTR]]) ]
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; USE_ASSUME-NEXT:    ret void
;

  %val0 = load i32, i32* %ptr, !invariant.load !{}
  call void @clobber_and_use(i32 %val0)
  %val1 = load i32, i32* %ptr
  call void @clobber_and_use(i32 %val1)
  ret void
}

define void @f_2(i32* %ptr) {
; We can forward a non-invariant load into an invariant load.
; NO_ASSUME-LABEL: @f_2(
; NO_ASSUME-NEXT:    [[VAL0:%.*]] = load i32, i32* [[PTR:%.*]]
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; NO_ASSUME-NEXT:    ret void
;
; USE_ASSUME-LABEL: @f_2(
; USE_ASSUME-NEXT:    [[VAL0:%.*]] = load i32, i32* [[PTR:%.*]]
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; USE_ASSUME-NEXT:    call void @llvm.assume(i1 true) [ "dereferenceable"(i32* [[PTR]], i64 4), "nonnull"(i32* [[PTR]]) ]
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; USE_ASSUME-NEXT:    ret void
;

  %val0 = load i32, i32* %ptr
  call void @clobber_and_use(i32 %val0)
  %val1 = load i32, i32* %ptr, !invariant.load !{}
  call void @clobber_and_use(i32 %val1)
  ret void
}

define void @f_3(i1 %cond, i32* %ptr) {
; NO_ASSUME-LABEL: @f_3(
; NO_ASSUME-NEXT:    [[VAL0:%.*]] = load i32, i32* [[PTR:%.*]], !invariant.load !0
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; NO_ASSUME-NEXT:    br i1 [[COND:%.*]], label [[LEFT:%.*]], label [[RIGHT:%.*]]
; NO_ASSUME:       left:
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; NO_ASSUME-NEXT:    ret void
; NO_ASSUME:       right:
; NO_ASSUME-NEXT:    ret void
;
; USE_ASSUME-LABEL: @f_3(
; USE_ASSUME-NEXT:    [[VAL0:%.*]] = load i32, i32* [[PTR:%.*]], !invariant.load !0
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; USE_ASSUME-NEXT:    br i1 [[COND:%.*]], label [[LEFT:%.*]], label [[RIGHT:%.*]]
; USE_ASSUME:       left:
; USE_ASSUME-NEXT:    call void @llvm.assume(i1 true) [ "dereferenceable"(i32* [[PTR]], i64 4), "nonnull"(i32* [[PTR]]) ]
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; USE_ASSUME-NEXT:    ret void
; USE_ASSUME:       right:
; USE_ASSUME-NEXT:    ret void
;
  %val0 = load i32, i32* %ptr, !invariant.load !{}
  call void @clobber_and_use(i32 %val0)
  br i1 %cond, label %left, label %right


left:
  %val1 = load i32, i32* %ptr
  call void @clobber_and_use(i32 %val1)
  ret void

right:
  ret void
}

define void @f_4(i1 %cond, i32* %ptr) {
; Negative test -- can't forward %val0 to %va1 because that'll break
; def-dominates-use.
; CHECK-LABEL: @f_4(
; CHECK-NEXT:    br i1 [[COND:%.*]], label [[LEFT:%.*]], label [[MERGE:%.*]]
; CHECK:       left:
; CHECK-NEXT:    [[VAL0:%.*]] = load i32, i32* [[PTR:%.*]], !invariant.load !0
; CHECK-NEXT:    call void @clobber_and_use(i32 [[VAL0]])
; CHECK-NEXT:    br label [[MERGE]]
; CHECK:       merge:
; CHECK-NEXT:    [[VAL1:%.*]] = load i32, i32* [[PTR]]
; CHECK-NEXT:    call void @clobber_and_use(i32 [[VAL1]])
; CHECK-NEXT:    ret void
;
  br i1 %cond, label %left, label %merge

left:

  %val0 = load i32, i32* %ptr, !invariant.load !{}
  call void @clobber_and_use(i32 %val0)
  br label %merge

merge:

  %val1 = load i32, i32* %ptr
  call void @clobber_and_use(i32 %val1)
  ret void
}

; By assumption, the call can't change contents of p
; LangRef is a bit unclear about whether the store is reachable, so
; for the moment we chose to be conservative and just assume it's valid
; to restore the same unchanging value.
define void @test_dse1(i32* %p) {
; NO_ASSUME-LABEL: @test_dse1(
; NO_ASSUME-NEXT:    [[V1:%.*]] = load i32, i32* [[P:%.*]], !invariant.load !0
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[V1]])
; NO_ASSUME-NEXT:    ret void
;
; USE_ASSUME-LABEL: @test_dse1(
; USE_ASSUME-NEXT:    [[V1:%.*]] = load i32, i32* [[P:%.*]], !invariant.load !0
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[V1]])
; USE_ASSUME-NEXT:    call void @llvm.assume(i1 true) [ "dereferenceable"(i32* [[P]], i64 4), "nonnull"(i32* [[P]]) ]
; USE_ASSUME-NEXT:    ret void
;
  %v1 = load i32, i32* %p, !invariant.load !{}
  call void @clobber_and_use(i32 %v1)
  store i32 %v1, i32* %p
  ret void
}

; By assumption, v1 must equal v2 (TODO)
define void @test_false_negative_dse2(i32* %p, i32 %v2) {
; CHECK-LABEL: @test_false_negative_dse2(
; CHECK-NEXT:    [[V1:%.*]] = load i32, i32* [[P:%.*]], !invariant.load !0
; CHECK-NEXT:    call void @clobber_and_use(i32 [[V1]])
; CHECK-NEXT:    store i32 [[V2:%.*]], i32* [[P]]
; CHECK-NEXT:    ret void
;
  %v1 = load i32, i32* %p, !invariant.load !{}
  call void @clobber_and_use(i32 %v1)
  store i32 %v2, i32* %p
  ret void
}

; If we remove the load, we still start an invariant scope since
; it lets us remove later loads not explicitly marked invariant
define void @test_scope_start_without_load(i32* %p) {
; NO_ASSUME-LABEL: @test_scope_start_without_load(
; NO_ASSUME-NEXT:    [[V1:%.*]] = load i32, i32* [[P:%.*]]
; NO_ASSUME-NEXT:    [[ADD:%.*]] = add i32 [[V1]], [[V1]]
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[ADD]])
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[V1]])
; NO_ASSUME-NEXT:    ret void
;
; USE_ASSUME-LABEL: @test_scope_start_without_load(
; USE_ASSUME-NEXT:    [[V1:%.*]] = load i32, i32* [[P:%.*]]
; USE_ASSUME-NEXT:    call void @llvm.assume(i1 true) [ "dereferenceable"(i32* [[P]], i64 4), "nonnull"(i32* [[P]]) ]
; USE_ASSUME-NEXT:    [[ADD:%.*]] = add i32 [[V1]], [[V1]]
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[ADD]])
; USE_ASSUME-NEXT:    call void @llvm.assume(i1 true) [ "dereferenceable"(i32* [[P]], i64 4), "nonnull"(i32* [[P]]) ]
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[V1]])
; USE_ASSUME-NEXT:    ret void
;
  %v1 = load i32, i32* %p
  %v2 = load i32, i32* %p, !invariant.load !{}
  %add = add i32 %v1, %v2
  call void @clobber_and_use(i32 %add)
  %v3 = load i32, i32* %p
  call void @clobber_and_use(i32 %v3)
  ret void
}

; If we already have an invariant scope, don't want to start a new one
; with a potentially greater generation.  This hides the earlier invariant
; load
define void @test_scope_restart(i32* %p) {
; NO_ASSUME-LABEL: @test_scope_restart(
; NO_ASSUME-NEXT:    [[V1:%.*]] = load i32, i32* [[P:%.*]], !invariant.load !0
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[V1]])
; NO_ASSUME-NEXT:    [[ADD:%.*]] = add i32 [[V1]], [[V1]]
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[ADD]])
; NO_ASSUME-NEXT:    call void @clobber_and_use(i32 [[V1]])
; NO_ASSUME-NEXT:    ret void
;
; USE_ASSUME-LABEL: @test_scope_restart(
; USE_ASSUME-NEXT:    [[V1:%.*]] = load i32, i32* [[P:%.*]], !invariant.load !0
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[V1]])
; USE_ASSUME-NEXT:    call void @llvm.assume(i1 true) [ "dereferenceable"(i32* [[P]], i64 4), "nonnull"(i32* [[P]]) ]
; USE_ASSUME-NEXT:    [[ADD:%.*]] = add i32 [[V1]], [[V1]]
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[ADD]])
; USE_ASSUME-NEXT:    call void @llvm.assume(i1 true) [ "dereferenceable"(i32* [[P]], i64 4), "nonnull"(i32* [[P]]) ]
; USE_ASSUME-NEXT:    call void @clobber_and_use(i32 [[V1]])
; USE_ASSUME-NEXT:    ret void
;
  %v1 = load i32, i32* %p, !invariant.load !{}
  call void @clobber_and_use(i32 %v1)
  %v2 = load i32, i32* %p, !invariant.load !{}
  %add = add i32 %v1, %v2
  call void @clobber_and_use(i32 %add)
  %v3 = load i32, i32* %p
  call void @clobber_and_use(i32 %v3)
  ret void
}
