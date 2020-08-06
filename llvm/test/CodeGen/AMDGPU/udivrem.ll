; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -march=r600 -mcpu=redwood < %s | FileCheck --check-prefix=R600 %s
; RUN: llc -march=amdgcn -mcpu=tahiti -verify-machineinstrs < %s | FileCheck --check-prefix=GFX6 %s
; RUN: llc -march=amdgcn -mcpu=tonga -verify-machineinstrs < %s | FileCheck --check-prefix=GFX8 %s

define amdgpu_kernel void @test_udivrem(i32 addrspace(1)* %out0, [8 x i32], i32 addrspace(1)* %out1, [8 x i32], i32 %x, [8 x i32], i32 %y) {
; R600-LABEL: test_udivrem:
; R600:       ; %bb.0:
; R600-NEXT:    ALU 21, @4, KC0[CB0:0-32], KC1[]
; R600-NEXT:    MEM_RAT_CACHELESS STORE_RAW T2.X, T3.X, 0
; R600-NEXT:    MEM_RAT_CACHELESS STORE_RAW T1.X, T0.X, 1
; R600-NEXT:    CF_END
; R600-NEXT:    ALU clause starting at 4:
; R600-NEXT:     SUB_INT T0.W, 0.0, KC0[9].X,
; R600-NEXT:     RECIP_UINT * T0.X, KC0[9].X,
; R600-NEXT:     MULLO_INT * T0.Y, PV.W, PS,
; R600-NEXT:     MULHI * T0.Y, T0.X, PS,
; R600-NEXT:     ADD_INT * T0.W, T0.X, PS,
; R600-NEXT:     MULHI * T0.X, KC0[6].W, PV.W,
; R600-NEXT:     MULLO_INT * T0.Y, PS, KC0[9].X,
; R600-NEXT:     SUB_INT * T0.W, KC0[6].W, PS,
; R600-NEXT:     SUB_INT T1.W, PV.W, KC0[9].X,
; R600-NEXT:     SETGE_UINT * T2.W, PV.W, KC0[9].X,
; R600-NEXT:     CNDE_INT * T0.W, PS, T0.W, PV.W,
; R600-NEXT:     ADD_INT T0.Z, T0.X, 1,
; R600-NEXT:     SUB_INT T1.W, PV.W, KC0[9].X,
; R600-NEXT:     SETGE_UINT * T3.W, PV.W, KC0[9].X,
; R600-NEXT:     CNDE_INT T1.X, PS, T0.W, PV.W,
; R600-NEXT:     CNDE_INT T0.W, T2.W, T0.X, PV.Z,
; R600-NEXT:     LSHR * T0.X, KC0[4].Z, literal.x,
; R600-NEXT:    2(2.802597e-45), 0(0.000000e+00)
; R600-NEXT:     ADD_INT * T1.W, PV.W, 1,
; R600-NEXT:     CNDE_INT T2.X, T3.W, T0.W, PV.W,
; R600-NEXT:     LSHR * T3.X, KC0[2].Y, literal.x,
; R600-NEXT:    2(2.802597e-45), 0(0.000000e+00)
;
; GFX6-LABEL: test_udivrem:
; GFX6:       ; %bb.0:
; GFX6-NEXT:        s_load_dword s3, s[0:1], 0x26
; GFX6-NEXT:        s_load_dwordx2 s[4:5], s[0:1], 0x9
; GFX6-NEXT:        s_load_dwordx2 s[8:9], s[0:1], 0x13
; GFX6-NEXT:        s_load_dword s0, s[0:1], 0x1d
; GFX6-NEXT:        s_mov_b32 s7, 0xf000
; GFX6-NEXT:        s_mov_b32 s6, -1
; GFX6-NEXT:        s_mov_b32 s10, s6
; GFX6-NEXT:        s_waitcnt lgkmcnt(0)
; GFX6-NEXT:        v_cvt_f32_u32_e32 v0, s3
; GFX6-NEXT:        s_sub_i32 s2, 0, s3
; GFX6-NEXT:        s_mov_b32 s11, s7
; GFX6-NEXT:        v_rcp_iflag_f32_e32 v0, v0
; GFX6-NEXT:        v_mul_f32_e32 v0, 0x4f7ffffe, v0
; GFX6-NEXT:        v_cvt_u32_f32_e32 v0, v0
; GFX6-NEXT:        v_mul_lo_u32 v1, s2, v0
; GFX6-NEXT:        v_mul_hi_u32 v1, v0, v1
; GFX6-NEXT:        v_add_i32_e32 v0, vcc, v1, v0
; GFX6-NEXT:        v_mul_hi_u32 v0, s0, v0
; GFX6-NEXT:        v_mul_lo_u32 v1, v0, s3
; GFX6-NEXT:        v_add_i32_e32 v2, vcc, 1, v0
; GFX6-NEXT:        v_sub_i32_e32 v1, vcc, s0, v1
; GFX6-NEXT:        v_cmp_le_u32_e64 s[0:1], s3, v1
; GFX6-NEXT:        v_cndmask_b32_e64 v0, v0, v2, s[0:1]
; GFX6-NEXT:        v_subrev_i32_e32 v2, vcc, s3, v1
; GFX6-NEXT:        v_cndmask_b32_e64 v1, v1, v2, s[0:1]
; GFX6-NEXT:        v_add_i32_e32 v2, vcc, 1, v0
; GFX6-NEXT:        v_cmp_le_u32_e64 s[0:1], s3, v1
; GFX6-NEXT:        v_cndmask_b32_e64 v0, v0, v2, s[0:1]
; GFX6-NEXT:        v_subrev_i32_e32 v2, vcc, s3, v1
; GFX6-NEXT:        buffer_store_dword v0, off, s[4:7], 0
; GFX6-NEXT:        s_waitcnt expcnt(0)
; GFX6-NEXT:        v_cndmask_b32_e64 v0, v1, v2, s[0:1]
; GFX6-NEXT:        buffer_store_dword v0, off, s[8:11], 0
; GFX6-NEXT:        s_endpgm
;
; GFX8-LABEL: test_udivrem:
; GFX8:       ; %bb.0:
; GFX8-NEXT:    s_load_dword s7, s[0:1], 0x98
; GFX8-NEXT:    s_load_dword s6, s[0:1], 0x74
; GFX8-NEXT:    s_waitcnt lgkmcnt(0)
; GFX8-NEXT:    v_cvt_f32_u32_e32 v0, s7
; GFX8-NEXT:    s_sub_i32 s2, 0, s7
; GFX8-NEXT:    v_rcp_iflag_f32_e32 v0, v0
; GFX8-NEXT:    v_mul_f32_e32 v0, 0x4f7ffffe, v0
; GFX8-NEXT:    v_cvt_u32_f32_e32 v0, v0
; GFX8-NEXT:    v_mul_lo_u32 v1, s2, v0
; GFX8-NEXT:    s_load_dwordx2 s[2:3], s[0:1], 0x24
; GFX8-NEXT:    s_load_dwordx2 s[4:5], s[0:1], 0x4c
; GFX8-NEXT:    v_mul_hi_u32 v1, v0, v1
; GFX8-NEXT:    v_add_u32_e32 v0, vcc, v1, v0
; GFX8-NEXT:    v_mul_hi_u32 v2, s6, v0
; GFX8-NEXT:    s_waitcnt lgkmcnt(0)
; GFX8-NEXT:    v_mov_b32_e32 v0, s2
; GFX8-NEXT:    v_mov_b32_e32 v1, s3
; GFX8-NEXT:    v_mul_lo_u32 v3, v2, s7
; GFX8-NEXT:    v_add_u32_e32 v4, vcc, 1, v2
; GFX8-NEXT:    v_sub_u32_e32 v3, vcc, s6, v3
; GFX8-NEXT:    v_cmp_le_u32_e64 s[0:1], s7, v3
; GFX8-NEXT:    v_cndmask_b32_e64 v2, v2, v4, s[0:1]
; GFX8-NEXT:    v_subrev_u32_e32 v4, vcc, s7, v3
; GFX8-NEXT:    v_cndmask_b32_e64 v3, v3, v4, s[0:1]
; GFX8-NEXT:    v_add_u32_e32 v4, vcc, 1, v2
; GFX8-NEXT:    v_cmp_le_u32_e64 s[0:1], s7, v3
; GFX8-NEXT:    v_cndmask_b32_e64 v2, v2, v4, s[0:1]
; GFX8-NEXT:    flat_store_dword v[0:1], v2
; GFX8-NEXT:    v_subrev_u32_e32 v4, vcc, s7, v3
; GFX8-NEXT:    v_mov_b32_e32 v0, s4
; GFX8-NEXT:    v_cndmask_b32_e64 v2, v3, v4, s[0:1]
; GFX8-NEXT:    v_mov_b32_e32 v1, s5
; GFX8-NEXT:    flat_store_dword v[0:1], v2
; GFX8-NEXT:    s_endpgm
  %result0 = udiv i32 %x, %y
  store i32 %result0, i32 addrspace(1)* %out0
  %result1 = urem i32 %x, %y
  store i32 %result1, i32 addrspace(1)* %out1
  ret void
}

define amdgpu_kernel void @test_udivrem_v2(<2 x i32> addrspace(1)* %out, <2 x i32> %x, <2 x i32> %y) {
; R600-LABEL: test_udivrem_v2:
; R600:       ; %bb.0:
; R600-NEXT:    ALU 29, @4, KC0[CB0:0-32], KC1[]
; R600-NEXT:    MEM_RAT_CACHELESS STORE_RAW T0.XY, T1.X, 1
; R600-NEXT:    CF_END
; R600-NEXT:    PAD
; R600-NEXT:    ALU clause starting at 4:
; R600-NEXT:     SUB_INT T0.W, 0.0, KC0[3].Z,
; R600-NEXT:     RECIP_UINT * T0.X, KC0[3].Z,
; R600-NEXT:     MULLO_INT * T0.Y, PV.W, PS,
; R600-NEXT:     SUB_INT T0.W, 0.0, KC0[3].Y,
; R600-NEXT:     RECIP_UINT * T0.Z, KC0[3].Y,
; R600-NEXT:     MULLO_INT * T0.W, PV.W, PS,
; R600-NEXT:     MULHI * T0.W, T0.Z, PS,
; R600-NEXT:     ADD_INT T0.W, T0.Z, PS,
; R600-NEXT:     MULHI * T0.Y, T0.X, T0.Y,
; R600-NEXT:     ADD_INT T1.W, T0.X, PS,
; R600-NEXT:     MULHI * T0.X, KC0[2].W, PV.W,
; R600-NEXT:     MULHI * T0.Y, KC0[3].X, PV.W,
; R600-NEXT:     MULLO_INT * T0.Y, PS, KC0[3].Z,
; R600-NEXT:     SUB_INT T0.W, KC0[3].X, PS,
; R600-NEXT:     MULLO_INT * T0.X, T0.X, KC0[3].Y,
; R600-NEXT:     SUB_INT T0.Z, KC0[2].W, PS,
; R600-NEXT:     SETGE_UINT T1.W, PV.W, KC0[3].Z,
; R600-NEXT:     SUB_INT * T2.W, PV.W, KC0[3].Z,
; R600-NEXT:     CNDE_INT T1.Z, PV.W, T0.W, PS,
; R600-NEXT:     SETGE_UINT T0.W, PV.Z, KC0[3].Y,
; R600-NEXT:     SUB_INT * T1.W, PV.Z, KC0[3].Y,
; R600-NEXT:     CNDE_INT T0.Z, PV.W, T0.Z, PS,
; R600-NEXT:     SETGE_UINT T0.W, PV.Z, KC0[3].Z,
; R600-NEXT:     SUB_INT * T1.W, PV.Z, KC0[3].Z,
; R600-NEXT:     CNDE_INT T0.Y, PV.W, T1.Z, PS,
; R600-NEXT:     SETGE_UINT T0.W, PV.Z, KC0[3].Y,
; R600-NEXT:     SUB_INT * T1.W, PV.Z, KC0[3].Y,
; R600-NEXT:     CNDE_INT T0.X, PV.W, T0.Z, PS,
; R600-NEXT:     LSHR * T1.X, KC0[2].Y, literal.x,
; R600-NEXT:    2(2.802597e-45), 0(0.000000e+00)
;
; GFX6-LABEL: test_udivrem_v2:
; GFX6:       ; %bb.0:
; GFX6-NEXT:    s_load_dwordx4 s[4:7], s[0:1], 0xb
; GFX6-NEXT:    s_mov_b32 s3, 0x4f7ffffe
; GFX6-NEXT:    s_load_dwordx2 s[0:1], s[0:1], 0x9
; GFX6-NEXT:    s_waitcnt lgkmcnt(0)
; GFX6-NEXT:    v_cvt_f32_u32_e32 v0, s6
; GFX6-NEXT:    s_sub_i32 s2, 0, s6
; GFX6-NEXT:    v_cvt_f32_u32_e32 v1, s7
; GFX6-NEXT:    v_rcp_iflag_f32_e32 v0, v0
; GFX6-NEXT:    v_rcp_iflag_f32_e32 v1, v1
; GFX6-NEXT:    v_mul_f32_e32 v0, s3, v0
; GFX6-NEXT:    v_cvt_u32_f32_e32 v0, v0
; GFX6-NEXT:    v_mul_f32_e32 v1, s3, v1
; GFX6-NEXT:    v_cvt_u32_f32_e32 v1, v1
; GFX6-NEXT:    s_mov_b32 s3, 0xf000
; GFX6-NEXT:    v_mul_lo_u32 v2, s2, v0
; GFX6-NEXT:    s_sub_i32 s2, 0, s7
; GFX6-NEXT:    v_mul_hi_u32 v2, v0, v2
; GFX6-NEXT:    v_add_i32_e32 v0, vcc, v2, v0
; GFX6-NEXT:    v_mul_hi_u32 v0, s4, v0
; GFX6-NEXT:    v_mul_lo_u32 v2, s2, v1
; GFX6-NEXT:    s_mov_b32 s2, -1
; GFX6-NEXT:    v_mul_lo_u32 v0, v0, s6
; GFX6-NEXT:    v_mul_hi_u32 v2, v1, v2
; GFX6-NEXT:    v_sub_i32_e32 v0, vcc, s4, v0
; GFX6-NEXT:    v_subrev_i32_e32 v3, vcc, s6, v0
; GFX6-NEXT:    v_cmp_le_u32_e32 vcc, s6, v0
; GFX6-NEXT:    v_cndmask_b32_e32 v0, v0, v3, vcc
; GFX6-NEXT:    v_subrev_i32_e32 v3, vcc, s6, v0
; GFX6-NEXT:    v_cmp_le_u32_e32 vcc, s6, v0
; GFX6-NEXT:    v_cndmask_b32_e32 v0, v0, v3, vcc
; GFX6-NEXT:    v_add_i32_e32 v1, vcc, v2, v1
; GFX6-NEXT:    v_mul_hi_u32 v1, s5, v1
; GFX6-NEXT:    v_mul_lo_u32 v1, v1, s7
; GFX6-NEXT:    v_sub_i32_e32 v1, vcc, s5, v1
; GFX6-NEXT:    v_subrev_i32_e32 v2, vcc, s7, v1
; GFX6-NEXT:    v_cmp_le_u32_e32 vcc, s7, v1
; GFX6-NEXT:    v_cndmask_b32_e32 v1, v1, v2, vcc
; GFX6-NEXT:    v_subrev_i32_e32 v2, vcc, s7, v1
; GFX6-NEXT:    v_cmp_le_u32_e32 vcc, s7, v1
; GFX6-NEXT:    v_cndmask_b32_e32 v1, v1, v2, vcc
; GFX6-NEXT:    buffer_store_dwordx2 v[0:1], off, s[0:3], 0
; GFX6-NEXT:    s_endpgm
;
; GFX8-LABEL: test_udivrem_v2:
; GFX8:       ; %bb.0:
; GFX8-NEXT:    s_load_dwordx4 s[4:7], s[0:1], 0x2c
; GFX8-NEXT:    s_mov_b32 s3, 0x4f7ffffe
; GFX8-NEXT:    s_load_dwordx2 s[0:1], s[0:1], 0x24
; GFX8-NEXT:    s_waitcnt lgkmcnt(0)
; GFX8-NEXT:    v_cvt_f32_u32_e32 v0, s6
; GFX8-NEXT:    s_sub_i32 s2, 0, s6
; GFX8-NEXT:    v_cvt_f32_u32_e32 v1, s7
; GFX8-NEXT:    v_rcp_iflag_f32_e32 v0, v0
; GFX8-NEXT:    v_rcp_iflag_f32_e32 v1, v1
; GFX8-NEXT:    v_mul_f32_e32 v0, s3, v0
; GFX8-NEXT:    v_cvt_u32_f32_e32 v0, v0
; GFX8-NEXT:    v_mul_f32_e32 v1, s3, v1
; GFX8-NEXT:    v_cvt_u32_f32_e32 v1, v1
; GFX8-NEXT:    v_mul_lo_u32 v2, s2, v0
; GFX8-NEXT:    s_sub_i32 s2, 0, s7
; GFX8-NEXT:    v_mul_hi_u32 v2, v0, v2
; GFX8-NEXT:    v_add_u32_e32 v0, vcc, v2, v0
; GFX8-NEXT:    v_mul_hi_u32 v0, s4, v0
; GFX8-NEXT:    v_mul_lo_u32 v2, s2, v1
; GFX8-NEXT:    v_mul_lo_u32 v0, v0, s6
; GFX8-NEXT:    v_mul_hi_u32 v2, v1, v2
; GFX8-NEXT:    v_sub_u32_e32 v0, vcc, s4, v0
; GFX8-NEXT:    v_subrev_u32_e32 v3, vcc, s6, v0
; GFX8-NEXT:    v_cmp_le_u32_e32 vcc, s6, v0
; GFX8-NEXT:    v_cndmask_b32_e32 v0, v0, v3, vcc
; GFX8-NEXT:    v_subrev_u32_e32 v3, vcc, s6, v0
; GFX8-NEXT:    v_cmp_le_u32_e32 vcc, s6, v0
; GFX8-NEXT:    v_cndmask_b32_e32 v0, v0, v3, vcc
; GFX8-NEXT:    v_add_u32_e32 v1, vcc, v2, v1
; GFX8-NEXT:    v_mul_hi_u32 v1, s5, v1
; GFX8-NEXT:    v_mul_lo_u32 v1, v1, s7
; GFX8-NEXT:    v_sub_u32_e32 v1, vcc, s5, v1
; GFX8-NEXT:    v_subrev_u32_e32 v2, vcc, s7, v1
; GFX8-NEXT:    v_cmp_le_u32_e32 vcc, s7, v1
; GFX8-NEXT:    v_cndmask_b32_e32 v1, v1, v2, vcc
; GFX8-NEXT:    v_subrev_u32_e32 v2, vcc, s7, v1
; GFX8-NEXT:    v_cmp_le_u32_e32 vcc, s7, v1
; GFX8-NEXT:    v_cndmask_b32_e32 v1, v1, v2, vcc
; GFX8-NEXT:    v_mov_b32_e32 v3, s1
; GFX8-NEXT:    v_mov_b32_e32 v2, s0
; GFX8-NEXT:    flat_store_dwordx2 v[2:3], v[0:1]
; GFX8-NEXT:    s_endpgm
  %result0 = udiv <2 x i32> %x, %y
  store <2 x i32> %result0, <2 x i32> addrspace(1)* %out
  %result1 = urem <2 x i32> %x, %y
  store <2 x i32> %result1, <2 x i32> addrspace(1)* %out
  ret void
}

define amdgpu_kernel void @test_udivrem_v4(<4 x i32> addrspace(1)* %out, <4 x i32> %x, <4 x i32> %y) {
; R600-LABEL: test_udivrem_v4:
; R600:       ; %bb.0:
; R600-NEXT:    ALU 57, @4, KC0[CB0:0-32], KC1[]
; R600-NEXT:    MEM_RAT_CACHELESS STORE_RAW T3.XYZW, T0.X, 1
; R600-NEXT:    CF_END
; R600-NEXT:    PAD
; R600-NEXT:    ALU clause starting at 4:
; R600-NEXT:     SUB_INT T0.W, 0.0, KC0[5].X,
; R600-NEXT:     RECIP_UINT * T0.X, KC0[5].X,
; R600-NEXT:     MULLO_INT * T0.Y, PV.W, PS,
; R600-NEXT:     SUB_INT T0.W, 0.0, KC0[4].Z,
; R600-NEXT:     RECIP_UINT * T0.Z, KC0[4].Z,
; R600-NEXT:     MULLO_INT * T0.W, PV.W, PS,
; R600-NEXT:     MULHI * T0.W, T0.Z, PS,
; R600-NEXT:     ADD_INT T0.W, T0.Z, PS,
; R600-NEXT:     MULHI * T0.Y, T0.X, T0.Y,
; R600-NEXT:     ADD_INT T1.W, T0.X, PS,
; R600-NEXT:     MULHI * T0.X, KC0[3].Z, PV.W,
; R600-NEXT:     MULHI * T0.Y, KC0[4].X, PV.W,
; R600-NEXT:     MULLO_INT * T0.Y, PS, KC0[5].X,
; R600-NEXT:     RECIP_UINT * T0.Z, KC0[4].Y,
; R600-NEXT:     SUB_INT T0.W, 0.0, KC0[4].W,
; R600-NEXT:     RECIP_UINT * T1.X, KC0[4].W,
; R600-NEXT:     MULLO_INT * T0.W, PV.W, PS,
; R600-NEXT:     SUB_INT T1.W, 0.0, KC0[4].Y,
; R600-NEXT:     MULHI * T0.W, T1.X, PS,
; R600-NEXT:     ADD_INT T0.W, T1.X, PS,
; R600-NEXT:     MULLO_INT * T1.X, PV.W, T0.Z,
; R600-NEXT:     MULHI * T0.W, KC0[3].W, PV.W,
; R600-NEXT:     MULLO_INT * T0.W, PS, KC0[4].W,
; R600-NEXT:     SUB_INT T0.W, KC0[3].W, PS,
; R600-NEXT:     MULHI * T1.X, T0.Z, T1.X,
; R600-NEXT:     SETGE_UINT T1.Y, PV.W, KC0[4].W,
; R600-NEXT:     ADD_INT T0.Z, T0.Z, PS,
; R600-NEXT:     SUB_INT T1.W, KC0[4].X, T0.Y,
; R600-NEXT:     MULLO_INT * T0.X, T0.X, KC0[4].Z,
; R600-NEXT:     SUB_INT T0.Y, KC0[3].Z, PS,
; R600-NEXT:     SETGE_UINT T1.Z, PV.W, KC0[5].X,
; R600-NEXT:     SUB_INT * T2.W, PV.W, KC0[5].X,
; R600-NEXT:     MULHI * T0.X, KC0[3].Y, T0.Z,
; R600-NEXT:     SUB_INT T1.X, T0.W, KC0[4].W,
; R600-NEXT:     CNDE_INT T2.Y, T1.Z, T1.W, T2.W,
; R600-NEXT:     SETGE_UINT T0.Z, T0.Y, KC0[4].Z,
; R600-NEXT:     SUB_INT T1.W, T0.Y, KC0[4].Z,
; R600-NEXT:     MULLO_INT * T0.X, PS, KC0[4].Y,
; R600-NEXT:     CNDE_INT T2.X, PV.Z, T0.Y, PV.W,
; R600-NEXT:     SETGE_UINT T0.Y, PV.Y, KC0[5].X,
; R600-NEXT:     SUB_INT T0.Z, PV.Y, KC0[5].X,
; R600-NEXT:     SUB_INT T1.W, KC0[3].Y, PS,
; R600-NEXT:     CNDE_INT * T0.W, T1.Y, T0.W, PV.X,
; R600-NEXT:     SETGE_UINT T0.X, PS, KC0[4].W,
; R600-NEXT:     SUB_INT T1.Y, PS, KC0[4].W,
; R600-NEXT:     SETGE_UINT T1.Z, PV.W, KC0[4].Y,
; R600-NEXT:     SUB_INT T2.W, PV.W, KC0[4].Y,
; R600-NEXT:     CNDE_INT * T3.W, PV.Y, T2.Y, PV.Z,
; R600-NEXT:     CNDE_INT T0.Y, PV.Z, T1.W, PV.W,
; R600-NEXT:     CNDE_INT T3.Z, PV.X, T0.W, PV.Y, BS:VEC_021/SCL_122
; R600-NEXT:     SETGE_UINT T0.W, T2.X, KC0[4].Z,
; R600-NEXT:     SUB_INT * T1.W, T2.X, KC0[4].Z,
; R600-NEXT:     CNDE_INT T3.Y, PV.W, T2.X, PS,
; R600-NEXT:     SETGE_UINT T0.W, PV.Y, KC0[4].Y,
; R600-NEXT:     SUB_INT * T1.W, PV.Y, KC0[4].Y,
; R600-NEXT:     CNDE_INT T3.X, PV.W, T0.Y, PS,
; R600-NEXT:     LSHR * T0.X, KC0[2].Y, literal.x,
; R600-NEXT:    2(2.802597e-45), 0(0.000000e+00)
;
; GFX6-LABEL: test_udivrem_v4:
; GFX6:       ; %bb.0:
; GFX6-NEXT:    s_load_dwordx8 s[4:11], s[0:1], 0xd
; GFX6-NEXT:    s_mov_b32 s12, 0x4f7ffffe
; GFX6-NEXT:    s_load_dwordx2 s[0:1], s[0:1], 0x9
; GFX6-NEXT:    s_waitcnt lgkmcnt(0)
; GFX6-NEXT:    v_cvt_f32_u32_e32 v0, s8
; GFX6-NEXT:    s_sub_i32 s2, 0, s8
; GFX6-NEXT:    v_cvt_f32_u32_e32 v1, s9
; GFX6-NEXT:    v_cvt_f32_u32_e32 v4, s11
; GFX6-NEXT:    v_rcp_iflag_f32_e32 v0, v0
; GFX6-NEXT:    s_sub_i32 s3, 0, s9
; GFX6-NEXT:    v_rcp_iflag_f32_e32 v1, v1
; GFX6-NEXT:    v_cvt_f32_u32_e32 v2, s10
; GFX6-NEXT:    v_mul_f32_e32 v0, s12, v0
; GFX6-NEXT:    v_cvt_u32_f32_e32 v0, v0
; GFX6-NEXT:    v_mul_f32_e32 v1, s12, v1
; GFX6-NEXT:    v_cvt_u32_f32_e32 v1, v1
; GFX6-NEXT:    v_rcp_iflag_f32_e32 v2, v2
; GFX6-NEXT:    v_mul_lo_u32 v3, s2, v0
; GFX6-NEXT:    s_sub_i32 s2, 0, s10
; GFX6-NEXT:    v_mul_f32_e32 v2, s12, v2
; GFX6-NEXT:    v_mul_hi_u32 v3, v0, v3
; GFX6-NEXT:    v_cvt_u32_f32_e32 v2, v2
; GFX6-NEXT:    v_add_i32_e32 v0, vcc, v3, v0
; GFX6-NEXT:    v_mul_hi_u32 v0, s4, v0
; GFX6-NEXT:    v_rcp_iflag_f32_e32 v3, v4
; GFX6-NEXT:    v_mul_lo_u32 v4, s3, v1
; GFX6-NEXT:    s_mov_b32 s3, 0xf000
; GFX6-NEXT:    v_mul_lo_u32 v0, v0, s8
; GFX6-NEXT:    v_mul_f32_e32 v3, s12, v3
; GFX6-NEXT:    v_mul_hi_u32 v4, v1, v4
; GFX6-NEXT:    v_cvt_u32_f32_e32 v3, v3
; GFX6-NEXT:    v_sub_i32_e32 v0, vcc, s4, v0
; GFX6-NEXT:    v_subrev_i32_e32 v5, vcc, s8, v0
; GFX6-NEXT:    v_cmp_le_u32_e32 vcc, s8, v0
; GFX6-NEXT:    v_cndmask_b32_e32 v0, v0, v5, vcc
; GFX6-NEXT:    v_subrev_i32_e32 v5, vcc, s8, v0
; GFX6-NEXT:    v_cmp_le_u32_e32 vcc, s8, v0
; GFX6-NEXT:    v_cndmask_b32_e32 v0, v0, v5, vcc
; GFX6-NEXT:    v_add_i32_e32 v1, vcc, v4, v1
; GFX6-NEXT:    v_mul_hi_u32 v1, s5, v1
; GFX6-NEXT:    v_mul_lo_u32 v4, s2, v2
; GFX6-NEXT:    s_sub_i32 s2, 0, s11
; GFX6-NEXT:    v_mul_lo_u32 v1, v1, s9
; GFX6-NEXT:    v_mul_hi_u32 v4, v2, v4
; GFX6-NEXT:    v_sub_i32_e32 v1, vcc, s5, v1
; GFX6-NEXT:    v_subrev_i32_e32 v5, vcc, s9, v1
; GFX6-NEXT:    v_cmp_le_u32_e32 vcc, s9, v1
; GFX6-NEXT:    v_cndmask_b32_e32 v1, v1, v5, vcc
; GFX6-NEXT:    v_subrev_i32_e32 v5, vcc, s9, v1
; GFX6-NEXT:    v_cmp_le_u32_e32 vcc, s9, v1
; GFX6-NEXT:    v_cndmask_b32_e32 v1, v1, v5, vcc
; GFX6-NEXT:    v_add_i32_e32 v2, vcc, v4, v2
; GFX6-NEXT:    v_mul_hi_u32 v2, s6, v2
; GFX6-NEXT:    v_mul_lo_u32 v4, s2, v3
; GFX6-NEXT:    s_mov_b32 s2, -1
; GFX6-NEXT:    v_mul_lo_u32 v2, v2, s10
; GFX6-NEXT:    v_mul_hi_u32 v4, v3, v4
; GFX6-NEXT:    v_sub_i32_e32 v2, vcc, s6, v2
; GFX6-NEXT:    v_subrev_i32_e32 v5, vcc, s10, v2
; GFX6-NEXT:    v_cmp_le_u32_e32 vcc, s10, v2
; GFX6-NEXT:    v_cndmask_b32_e32 v2, v2, v5, vcc
; GFX6-NEXT:    v_subrev_i32_e32 v5, vcc, s10, v2
; GFX6-NEXT:    v_cmp_le_u32_e32 vcc, s10, v2
; GFX6-NEXT:    v_cndmask_b32_e32 v2, v2, v5, vcc
; GFX6-NEXT:    v_add_i32_e32 v3, vcc, v4, v3
; GFX6-NEXT:    v_mul_hi_u32 v3, s7, v3
; GFX6-NEXT:    v_mul_lo_u32 v3, v3, s11
; GFX6-NEXT:    v_sub_i32_e32 v3, vcc, s7, v3
; GFX6-NEXT:    v_subrev_i32_e32 v4, vcc, s11, v3
; GFX6-NEXT:    v_cmp_le_u32_e32 vcc, s11, v3
; GFX6-NEXT:    v_cndmask_b32_e32 v3, v3, v4, vcc
; GFX6-NEXT:    v_subrev_i32_e32 v4, vcc, s11, v3
; GFX6-NEXT:    v_cmp_le_u32_e32 vcc, s11, v3
; GFX6-NEXT:    v_cndmask_b32_e32 v3, v3, v4, vcc
; GFX6-NEXT:    buffer_store_dwordx4 v[0:3], off, s[0:3], 0
; GFX6-NEXT:    s_endpgm
;
; GFX8-LABEL: test_udivrem_v4:
; GFX8:       ; %bb.0:
; GFX8-NEXT:    s_load_dwordx8 s[4:11], s[0:1], 0x34
; GFX8-NEXT:    s_mov_b32 s12, 0x4f7ffffe
; GFX8-NEXT:    s_load_dwordx2 s[0:1], s[0:1], 0x24
; GFX8-NEXT:    s_waitcnt lgkmcnt(0)
; GFX8-NEXT:    v_cvt_f32_u32_e32 v0, s8
; GFX8-NEXT:    s_sub_i32 s2, 0, s8
; GFX8-NEXT:    v_cvt_f32_u32_e32 v1, s9
; GFX8-NEXT:    v_cvt_f32_u32_e32 v4, s11
; GFX8-NEXT:    v_rcp_iflag_f32_e32 v0, v0
; GFX8-NEXT:    s_sub_i32 s3, 0, s9
; GFX8-NEXT:    v_rcp_iflag_f32_e32 v1, v1
; GFX8-NEXT:    v_cvt_f32_u32_e32 v2, s10
; GFX8-NEXT:    v_mul_f32_e32 v0, s12, v0
; GFX8-NEXT:    v_cvt_u32_f32_e32 v0, v0
; GFX8-NEXT:    v_mul_f32_e32 v1, s12, v1
; GFX8-NEXT:    v_cvt_u32_f32_e32 v1, v1
; GFX8-NEXT:    v_rcp_iflag_f32_e32 v2, v2
; GFX8-NEXT:    v_mul_lo_u32 v3, s2, v0
; GFX8-NEXT:    s_sub_i32 s2, 0, s10
; GFX8-NEXT:    v_mul_f32_e32 v2, s12, v2
; GFX8-NEXT:    v_mul_hi_u32 v3, v0, v3
; GFX8-NEXT:    v_cvt_u32_f32_e32 v2, v2
; GFX8-NEXT:    v_add_u32_e32 v0, vcc, v3, v0
; GFX8-NEXT:    v_mul_hi_u32 v0, s4, v0
; GFX8-NEXT:    v_rcp_iflag_f32_e32 v3, v4
; GFX8-NEXT:    v_mul_lo_u32 v4, s3, v1
; GFX8-NEXT:    v_mul_lo_u32 v0, v0, s8
; GFX8-NEXT:    v_mul_f32_e32 v3, s12, v3
; GFX8-NEXT:    v_mul_hi_u32 v4, v1, v4
; GFX8-NEXT:    v_cvt_u32_f32_e32 v3, v3
; GFX8-NEXT:    v_sub_u32_e32 v0, vcc, s4, v0
; GFX8-NEXT:    v_subrev_u32_e32 v5, vcc, s8, v0
; GFX8-NEXT:    v_cmp_le_u32_e32 vcc, s8, v0
; GFX8-NEXT:    v_cndmask_b32_e32 v0, v0, v5, vcc
; GFX8-NEXT:    v_subrev_u32_e32 v5, vcc, s8, v0
; GFX8-NEXT:    v_cmp_le_u32_e32 vcc, s8, v0
; GFX8-NEXT:    v_cndmask_b32_e32 v0, v0, v5, vcc
; GFX8-NEXT:    v_add_u32_e32 v1, vcc, v4, v1
; GFX8-NEXT:    v_mul_hi_u32 v1, s5, v1
; GFX8-NEXT:    v_mul_lo_u32 v4, s2, v2
; GFX8-NEXT:    s_sub_i32 s2, 0, s11
; GFX8-NEXT:    v_mul_lo_u32 v1, v1, s9
; GFX8-NEXT:    v_mul_hi_u32 v4, v2, v4
; GFX8-NEXT:    v_sub_u32_e32 v1, vcc, s5, v1
; GFX8-NEXT:    v_subrev_u32_e32 v5, vcc, s9, v1
; GFX8-NEXT:    v_cmp_le_u32_e32 vcc, s9, v1
; GFX8-NEXT:    v_cndmask_b32_e32 v1, v1, v5, vcc
; GFX8-NEXT:    v_subrev_u32_e32 v5, vcc, s9, v1
; GFX8-NEXT:    v_cmp_le_u32_e32 vcc, s9, v1
; GFX8-NEXT:    v_cndmask_b32_e32 v1, v1, v5, vcc
; GFX8-NEXT:    v_add_u32_e32 v2, vcc, v4, v2
; GFX8-NEXT:    v_mul_hi_u32 v2, s6, v2
; GFX8-NEXT:    v_mul_lo_u32 v4, s2, v3
; GFX8-NEXT:    v_mul_lo_u32 v2, v2, s10
; GFX8-NEXT:    v_mul_hi_u32 v4, v3, v4
; GFX8-NEXT:    v_sub_u32_e32 v2, vcc, s6, v2
; GFX8-NEXT:    v_subrev_u32_e32 v5, vcc, s10, v2
; GFX8-NEXT:    v_cmp_le_u32_e32 vcc, s10, v2
; GFX8-NEXT:    v_cndmask_b32_e32 v2, v2, v5, vcc
; GFX8-NEXT:    v_subrev_u32_e32 v5, vcc, s10, v2
; GFX8-NEXT:    v_cmp_le_u32_e32 vcc, s10, v2
; GFX8-NEXT:    v_cndmask_b32_e32 v2, v2, v5, vcc
; GFX8-NEXT:    v_add_u32_e32 v3, vcc, v4, v3
; GFX8-NEXT:    v_mul_hi_u32 v3, s7, v3
; GFX8-NEXT:    v_mul_lo_u32 v3, v3, s11
; GFX8-NEXT:    v_sub_u32_e32 v3, vcc, s7, v3
; GFX8-NEXT:    v_subrev_u32_e32 v4, vcc, s11, v3
; GFX8-NEXT:    v_cmp_le_u32_e32 vcc, s11, v3
; GFX8-NEXT:    v_cndmask_b32_e32 v3, v3, v4, vcc
; GFX8-NEXT:    v_subrev_u32_e32 v4, vcc, s11, v3
; GFX8-NEXT:    v_cmp_le_u32_e32 vcc, s11, v3
; GFX8-NEXT:    v_cndmask_b32_e32 v3, v3, v4, vcc
; GFX8-NEXT:    v_mov_b32_e32 v5, s1
; GFX8-NEXT:    v_mov_b32_e32 v4, s0
; GFX8-NEXT:    flat_store_dwordx4 v[4:5], v[0:3]
; GFX8-NEXT:    s_endpgm
  %result0 = udiv <4 x i32> %x, %y
  store <4 x i32> %result0, <4 x i32> addrspace(1)* %out
  %result1 = urem <4 x i32> %x, %y
  store <4 x i32> %result1, <4 x i32> addrspace(1)* %out
  ret void
}
