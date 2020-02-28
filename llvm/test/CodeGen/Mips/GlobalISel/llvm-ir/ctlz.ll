; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -O0 -mtriple=mipsel-linux-gnu -global-isel -verify-machineinstrs %s -o -| FileCheck %s -check-prefixes=MIPS32

define i32 @ctlz_i32(i32 %a) {
; MIPS32-LABEL: ctlz_i32:
; MIPS32:       # %bb.0: # %entry
; MIPS32-NEXT:    clz $2, $4
; MIPS32-NEXT:    jr $ra
; MIPS32-NEXT:    nop
entry:
  %0 = call i32 @llvm.ctlz.i32(i32 %a, i1 false)
  ret i32 %0
}
declare i32 @llvm.ctlz.i32(i32, i1 immarg)


define i64 @ctlz_i64(i64 %a) {
; MIPS32-LABEL: ctlz_i64:
; MIPS32:       # %bb.0: # %entry
; MIPS32-NEXT:    ori $3, $zero, 0
; MIPS32-NEXT:    sltiu $1, $5, 1
; MIPS32-NEXT:    clz $2, $4
; MIPS32-NEXT:    addiu $2, $2, 32
; MIPS32-NEXT:    clz $4, $5
; MIPS32-NEXT:    andi $1, $1, 1
; MIPS32-NEXT:    movn $4, $2, $1
; MIPS32-NEXT:    move $2, $4
; MIPS32-NEXT:    jr $ra
; MIPS32-NEXT:    nop
entry:
  %0 = call i64 @llvm.ctlz.i64(i64 %a, i1 false)
  ret i64 %0
}
declare i64 @llvm.ctlz.i64(i64, i1 immarg)
