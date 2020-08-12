// RUN: llvm-mc -arch=amdgcn -mcpu=gfx1030 -show-encoding %s | FileCheck --check-prefix=GFX10 %s
// RUN: llvm-mc -arch=amdgcn -mcpu=gfx1031 -show-encoding %s | FileCheck --check-prefix=GFX10 %s

global_load_dword_addtid v1, s[2:3] offset:16
// GFX10: encoding: [0x10,0x80,0x58,0xdc,0x00,0x00,0x02,0x01]

global_load_dword_addtid v1, s[2:3] offset:16 glc slc dlc
// GFX10: encoding: [0x10,0x90,0x5b,0xdc,0x00,0x00,0x02,0x01]

global_store_dword_addtid v1, s[2:3] offset:16 glc slc dlc
// GFX10: encoding: [0x10,0x90,0x5f,0xdc,0x00,0x01,0x02,0x00]

global_store_dword v[254:255], v1, s[2:3] offset:16
// GFX10: encoding: [0x10,0x80,0x70,0xdc,0xfe,0x01,0x02,0x00]

global_atomic_csub v2, v[0:1], v2, off offset:100 glc slc
// GFX10: encoding: [0x64,0x80,0xd3,0xdc,0x00,0x02,0x7d,0x02]

global_atomic_csub v2, v[0:1], v2, off
// GFX10: encoding: [0x00,0x80,0xd1,0xdc,0x00,0x02,0x7d,0x02]

global_atomic_csub v2, v[0:1], v2, s[2:3]
// GFX10: encoding: [0x00,0x80,0xd1,0xdc,0x00,0x02,0x02,0x02]

global_atomic_csub v2, v[0:1], v2, s[2:3] offset:100 glc slc
// GFX10: encoding: [0x64,0x80,0xd3,0xdc,0x00,0x02,0x02,0x02]

buffer_atomic_csub v5, off, s[8:11], s3
// GFX10: encoding: [0x00,0x40,0xd0,0xe0,0x00,0x05,0x02,0x03]

buffer_atomic_csub v5, off, s[8:11], s3 offset:4095 glc
// GFX10: encoding: [0xff,0x4f,0xd0,0xe0,0x00,0x05,0x02,0x03]

buffer_atomic_csub v5, off, s[8:11], -1 offset:4095 glc
// GFX10: encoding: [0xff,0x4f,0xd0,0xe0,0x00,0x05,0x02,0xc1]

buffer_atomic_csub v5, v0, s[8:11], s3 offen offset:4095 glc
// GFX10: encoding: [0xff,0x5f,0xd0,0xe0,0x00,0x05,0x02,0x03]

buffer_atomic_csub v5, v0, s[8:11], s3 idxen offset:4095 glc
// GFX10: encoding: [0xff,0x6f,0xd0,0xe0,0x00,0x05,0x02,0x03]

buffer_atomic_csub v5, off, s[8:11], s3 glc slc
// GFX10: encoding: [0x00,0x40,0xd0,0xe0,0x00,0x05,0x42,0x03]

s_getreg_b32 s2, hwreg(HW_REG_SHADER_CYCLES)
// GFX10: encoding: [0x1d,0xf8,0x02,0xb9]

s_getreg_b32 s2, 29
// GFX10: s_getreg_b32 s2, hwreg(HW_REG_SHADER_CYCLES, 0, 1) ; encoding: [0x1d,0x00,0x02,0xb9]

s_getreg_b32 s2, hwreg(22)
// GFX10: s_getreg_b32 s2, hwreg(22) ; encoding: [0x16,0xf8,0x02,0xb9]

v_fma_legacy_f32 v0, v1, v2, v3
// GFX10: encoding: [0x00,0x00,0x40,0xd5,0x01,0x05,0x0e,0x04]

v_fma_legacy_f32 v0, v1, |v2|, -v3
// GFX10: encoding: [0x00,0x02,0x40,0xd5,0x01,0x05,0x0e,0x84]

v_fma_legacy_f32 v0, s1, 2.0, -v3
// GFX10: encoding: [0x00,0x00,0x40,0xd5,0x01,0xe8,0x0d,0x84]

image_msaa_load v[1:4], v5, s[8:15] dmask:0xf dim:SQ_RSRC_IMG_1D
// GFX10: encoding: [0x01,0x0f,0x00,0xf0,0x05,0x01,0x02,0x00]

image_msaa_load v[1:4], v5, s[8:15] dmask:0xf dim:SQ_RSRC_IMG_1D glc
// GFX10: encoding: [0x01,0x2f,0x00,0xf0,0x05,0x01,0x02,0x00]

image_msaa_load v5, v[1:2], s[8:15] dmask:0x1 dim:SQ_RSRC_IMG_2D d16
// GFX10: encoding: [0x09,0x01,0x00,0xf0,0x01,0x05,0x02,0x80]

image_msaa_load v[1:4], v5, s[8:15] dmask:0xf dim:SQ_RSRC_IMG_1D
// GFX10: encoding: [0x01,0x0f,0x00,0xf0,0x05,0x01,0x02,0x00]

image_msaa_load v14, [v204,v11,v14,v19], s[40:47] dmask:0x1 dim:SQ_RSRC_IMG_2D_MSAA_ARRAY
// GFX10: encoding: [0x3b,0x01,0x00,0xf0,0xcc,0x0e,0x0a,0x00,0x0b,0x0e,0x13,0x00]
