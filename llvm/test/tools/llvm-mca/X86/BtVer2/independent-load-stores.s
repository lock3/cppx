# NOTE: Assertions have been autogenerated by utils/update_mca_test_checks.py
# RUN: llvm-mca -mtriple=x86_64-unknown-unknown -mcpu=btver2 -timeline -timeline-max-iterations=1 < %s | FileCheck %s -check-prefixes=ALL,NOALIAS
# RUN: llvm-mca -mtriple=x86_64-unknown-unknown -mcpu=btver2 -timeline -timeline-max-iterations=1 -noalias=false < %s | FileCheck %s -check-prefixes=ALL,YESALIAS

  addq	$44, 64(%r14)
  addq	$44, 128(%r14)
  addq	$44, 192(%r14)
  addq	$44, 256(%r14)
  addq	$44, 320(%r14)
  addq	$44, 384(%r14)
  addq	$44, 448(%r14)
  addq	$44, 512(%r14)
  addq	$44, 576(%r14)
  addq	$44, 640(%r14)

# ALL:           Iterations:        100
# ALL-NEXT:      Instructions:      1000

# NOALIAS-NEXT:  Total Cycles:      1008
# YESALIAS-NEXT: Total Cycles:      6003

# ALL-NEXT:      Total uOps:        1000

# ALL:           Dispatch Width:    2

# NOALIAS-NEXT:  uOps Per Cycle:    0.99
# NOALIAS-NEXT:  IPC:               0.99

# YESALIAS-NEXT: uOps Per Cycle:    0.17
# YESALIAS-NEXT: IPC:               0.17

# ALL-NEXT:      Block RThroughput: 10.0

# ALL:           Instruction Info:
# ALL-NEXT:      [1]: #uOps
# ALL-NEXT:      [2]: Latency
# ALL-NEXT:      [3]: RThroughput
# ALL-NEXT:      [4]: MayLoad
# ALL-NEXT:      [5]: MayStore
# ALL-NEXT:      [6]: HasSideEffects (U)

# ALL:           [1]    [2]    [3]    [4]    [5]    [6]    Instructions:
# ALL-NEXT:       1      6     1.00    *      *            addq	$44, 64(%r14)
# ALL-NEXT:       1      6     1.00    *      *            addq	$44, 128(%r14)
# ALL-NEXT:       1      6     1.00    *      *            addq	$44, 192(%r14)
# ALL-NEXT:       1      6     1.00    *      *            addq	$44, 256(%r14)
# ALL-NEXT:       1      6     1.00    *      *            addq	$44, 320(%r14)
# ALL-NEXT:       1      6     1.00    *      *            addq	$44, 384(%r14)
# ALL-NEXT:       1      6     1.00    *      *            addq	$44, 448(%r14)
# ALL-NEXT:       1      6     1.00    *      *            addq	$44, 512(%r14)
# ALL-NEXT:       1      6     1.00    *      *            addq	$44, 576(%r14)
# ALL-NEXT:       1      6     1.00    *      *            addq	$44, 640(%r14)

# ALL:           Resources:
# ALL-NEXT:      [0]   - JALU0
# ALL-NEXT:      [1]   - JALU1
# ALL-NEXT:      [2]   - JDiv
# ALL-NEXT:      [3]   - JFPA
# ALL-NEXT:      [4]   - JFPM
# ALL-NEXT:      [5]   - JFPU0
# ALL-NEXT:      [6]   - JFPU1
# ALL-NEXT:      [7]   - JLAGU
# ALL-NEXT:      [8]   - JMul
# ALL-NEXT:      [9]   - JSAGU
# ALL-NEXT:      [10]  - JSTC
# ALL-NEXT:      [11]  - JVALU0
# ALL-NEXT:      [12]  - JVALU1
# ALL-NEXT:      [13]  - JVIMUL

# ALL:           Resource pressure per iteration:
# ALL-NEXT:      [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]    [8]    [9]    [10]   [11]   [12]   [13]
# ALL-NEXT:      5.00   5.00    -      -      -      -      -     10.00   -     10.00   -      -      -      -

# ALL:           Resource pressure by instruction:
# ALL-NEXT:      [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]    [8]    [9]    [10]   [11]   [12]   [13]   Instructions:
# ALL-NEXT:       -     1.00    -      -      -      -      -     1.00    -     1.00    -      -      -      -     addq	$44, 64(%r14)
# ALL-NEXT:      1.00    -      -      -      -      -      -     1.00    -     1.00    -      -      -      -     addq	$44, 128(%r14)
# ALL-NEXT:       -     1.00    -      -      -      -      -     1.00    -     1.00    -      -      -      -     addq	$44, 192(%r14)
# ALL-NEXT:      1.00    -      -      -      -      -      -     1.00    -     1.00    -      -      -      -     addq	$44, 256(%r14)
# ALL-NEXT:       -     1.00    -      -      -      -      -     1.00    -     1.00    -      -      -      -     addq	$44, 320(%r14)
# ALL-NEXT:      1.00    -      -      -      -      -      -     1.00    -     1.00    -      -      -      -     addq	$44, 384(%r14)
# ALL-NEXT:       -     1.00    -      -      -      -      -     1.00    -     1.00    -      -      -      -     addq	$44, 448(%r14)
# ALL-NEXT:      1.00    -      -      -      -      -      -     1.00    -     1.00    -      -      -      -     addq	$44, 512(%r14)
# ALL-NEXT:       -     1.00    -      -      -      -      -     1.00    -     1.00    -      -      -      -     addq	$44, 576(%r14)
# ALL-NEXT:      1.00    -      -      -      -      -      -     1.00    -     1.00    -      -      -      -     addq	$44, 640(%r14)

# ALL:           Timeline view:

# NOALIAS-NEXT:                      01234567
# NOALIAS-NEXT:  Index     0123456789

# YESALIAS-NEXT:                     0123456789          0123456789          0123456789
# YESALIAS-NEXT: Index     0123456789          0123456789          0123456789          012

# NOALIAS:       [0,0]     DeeeeeeER .    . .   addq	$44, 64(%r14)
# NOALIAS-NEXT:  [0,1]     D=eeeeeeER.    . .   addq	$44, 128(%r14)
# NOALIAS-NEXT:  [0,2]     .D=eeeeeeER    . .   addq	$44, 192(%r14)
# NOALIAS-NEXT:  [0,3]     .D==eeeeeeER   . .   addq	$44, 256(%r14)
# NOALIAS-NEXT:  [0,4]     . D==eeeeeeER  . .   addq	$44, 320(%r14)
# NOALIAS-NEXT:  [0,5]     . D===eeeeeeER . .   addq	$44, 384(%r14)
# NOALIAS-NEXT:  [0,6]     .  D===eeeeeeER. .   addq	$44, 448(%r14)
# NOALIAS-NEXT:  [0,7]     .  D====eeeeeeER .   addq	$44, 512(%r14)
# NOALIAS-NEXT:  [0,8]     .   D====eeeeeeER.   addq	$44, 576(%r14)
# NOALIAS-NEXT:  [0,9]     .   D=====eeeeeeER   addq	$44, 640(%r14)

# YESALIAS:      [0,0]     DeeeeeeER .    .    .    .    .    .    .    .    .    .    . .   addq	$44, 64(%r14)
# YESALIAS-NEXT: [0,1]     D======eeeeeeER.    .    .    .    .    .    .    .    .    . .   addq	$44, 128(%r14)
# YESALIAS-NEXT: [0,2]     .D===========eeeeeeER    .    .    .    .    .    .    .    . .   addq	$44, 192(%r14)
# YESALIAS-NEXT: [0,3]     .D=================eeeeeeER   .    .    .    .    .    .    . .   addq	$44, 256(%r14)
# YESALIAS-NEXT: [0,4]     . D======================eeeeeeER  .    .    .    .    .    . .   addq	$44, 320(%r14)
# YESALIAS-NEXT: [0,5]     . D============================eeeeeeER .    .    .    .    . .   addq	$44, 384(%r14)
# YESALIAS-NEXT: [0,6]     .  D=================================eeeeeeER.    .    .    . .   addq	$44, 448(%r14)
# YESALIAS-NEXT: [0,7]     .  D=======================================eeeeeeER    .    . .   addq	$44, 512(%r14)
# YESALIAS-NEXT: [0,8]     .   D============================================eeeeeeER   . .   addq	$44, 576(%r14)
# YESALIAS-NEXT: [0,9]     .   D==================================================eeeeeeER   addq	$44, 640(%r14)

# ALL:           Average Wait times (based on the timeline view):
# ALL-NEXT:      [0]: Executions
# ALL-NEXT:      [1]: Average time spent waiting in a scheduler's queue
# ALL-NEXT:      [2]: Average time spent waiting in a scheduler's queue while ready
# ALL-NEXT:      [3]: Average time elapsed from WB until retire stage

# ALL:                 [0]    [1]    [2]    [3]
# ALL-NEXT:      0.     1     1.0    1.0    0.0       addq	$44, 64(%r14)

# NOALIAS-NEXT:  1.     1     2.0    1.0    0.0       addq	$44, 128(%r14)
# NOALIAS-NEXT:  2.     1     2.0    1.0    0.0       addq	$44, 192(%r14)
# NOALIAS-NEXT:  3.     1     3.0    1.0    0.0       addq	$44, 256(%r14)
# NOALIAS-NEXT:  4.     1     3.0    1.0    0.0       addq	$44, 320(%r14)
# NOALIAS-NEXT:  5.     1     4.0    1.0    0.0       addq	$44, 384(%r14)
# NOALIAS-NEXT:  6.     1     4.0    1.0    0.0       addq	$44, 448(%r14)
# NOALIAS-NEXT:  7.     1     5.0    1.0    0.0       addq	$44, 512(%r14)
# NOALIAS-NEXT:  8.     1     5.0    1.0    0.0       addq	$44, 576(%r14)
# NOALIAS-NEXT:  9.     1     6.0    1.0    0.0       addq	$44, 640(%r14)
# NOALIAS-NEXT:         1     3.5    1.0    0.0       <total>

# YESALIAS-NEXT: 1.     1     7.0    0.0    0.0       addq	$44, 128(%r14)
# YESALIAS-NEXT: 2.     1     12.0   0.0    0.0       addq	$44, 192(%r14)
# YESALIAS-NEXT: 3.     1     18.0   0.0    0.0       addq	$44, 256(%r14)
# YESALIAS-NEXT: 4.     1     23.0   0.0    0.0       addq	$44, 320(%r14)
# YESALIAS-NEXT: 5.     1     29.0   0.0    0.0       addq	$44, 384(%r14)
# YESALIAS-NEXT: 6.     1     34.0   0.0    0.0       addq	$44, 448(%r14)
# YESALIAS-NEXT: 7.     1     40.0   0.0    0.0       addq	$44, 512(%r14)
# YESALIAS-NEXT: 8.     1     45.0   0.0    0.0       addq	$44, 576(%r14)
# YESALIAS-NEXT: 9.     1     51.0   0.0    0.0       addq	$44, 640(%r14)
# YESALIAS-NEXT:        1     26.0   0.1    0.0       <total>
