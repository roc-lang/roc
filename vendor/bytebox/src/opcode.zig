const std = @import("std");
const common = @import("common.zig");

// A compressed version of the wasm opcodes for better table-oriented lookup (no holes). See WasmOpcode for the actual wasm representation.
pub const Opcode = enum(u16) {
    Invalid, // Has no corresponding mapping in WasmOpcode.
    Unreachable,
    DebugTrap, // Has no corresponding mapping in WasmOpcode, intended for use in returning control flow to invoker
    Noop,
    Block,
    Loop,
    If,
    IfNoElse, // variant of If that assumes no else branch
    Else,
    End,
    Branch,
    Branch_If,
    Branch_Table,
    Return,
    Call_Local, // Technically mapped to WasmOpcode.Call, but has different behavior (module-internal calls only)
    Call_Import, // Has no corresponding mapping in WasmOpcode, only calls imported functions
    Call_Indirect,
    Drop,
    Drop_V128, // Has no corresponding mapping in WasmOpcode
    Select,
    Select_T,
    Select_V128,
    Local_Get,
    Local_Set,
    Local_Tee,
    Local_Get_V128, // Has no corresponding mapping in WasmOpcode
    Local_Set_V128, // Has no corresponding mapping in WasmOpcode
    Local_Tee_V128, // Has no corresponding mapping in WasmOpcode
    Global_Get,
    Global_Set,
    Global_Get_V128, // Has no corresponding mapping in WasmOpcode
    Global_Set_V128, // Has no corresponding mapping in WasmOpcode
    Table_Get,
    Table_Set,
    I32_Load,
    I64_Load,
    F32_Load,
    F64_Load,
    I32_Load8_S,
    I32_Load8_U,
    I32_Load16_S,
    I32_Load16_U,
    I64_Load8_S,
    I64_Load8_U,
    I64_Load16_S,
    I64_Load16_U,
    I64_Load32_S,
    I64_Load32_U,
    I32_Store,
    I64_Store,
    F32_Store,
    F64_Store,
    I32_Store8,
    I32_Store16,
    I64_Store8,
    I64_Store16,
    I64_Store32,
    Memory_Size,
    Memory_Grow,
    I32_Const,
    I64_Const,
    F32_Const,
    F64_Const,
    I32_Eqz,
    I32_Eq,
    I32_NE,
    I32_LT_S,
    I32_LT_U,
    I32_GT_S,
    I32_GT_U,
    I32_LE_S,
    I32_LE_U,
    I32_GE_S,
    I32_GE_U,
    I64_Eqz,
    I64_Eq,
    I64_NE,
    I64_LT_S,
    I64_LT_U,
    I64_GT_S,
    I64_GT_U,
    I64_LE_S,
    I64_LE_U,
    I64_GE_S,
    I64_GE_U,
    F32_EQ,
    F32_NE,
    F32_LT,
    F32_GT,
    F32_LE,
    F32_GE,
    F64_EQ,
    F64_NE,
    F64_LT,
    F64_GT,
    F64_LE,
    F64_GE,
    I32_Clz,
    I32_Ctz,
    I32_Popcnt,
    I32_Add,
    I32_Sub,
    I32_Mul,
    I32_Div_S,
    I32_Div_U,
    I32_Rem_S,
    I32_Rem_U,
    I32_And,
    I32_Or,
    I32_Xor,
    I32_Shl,
    I32_Shr_S,
    I32_Shr_U,
    I32_Rotl,
    I32_Rotr,
    I64_Clz,
    I64_Ctz,
    I64_Popcnt,
    I64_Add,
    I64_Sub,
    I64_Mul,
    I64_Div_S,
    I64_Div_U,
    I64_Rem_S,
    I64_Rem_U,
    I64_And,
    I64_Or,
    I64_Xor,
    I64_Shl,
    I64_Shr_S,
    I64_Shr_U,
    I64_Rotl,
    I64_Rotr,
    F32_Abs,
    F32_Neg,
    F32_Ceil,
    F32_Floor,
    F32_Trunc,
    F32_Nearest,
    F32_Sqrt,
    F32_Add,
    F32_Sub,
    F32_Mul,
    F32_Div,
    F32_Min,
    F32_Max,
    F32_Copysign,
    F64_Abs,
    F64_Neg,
    F64_Ceil,
    F64_Floor,
    F64_Trunc,
    F64_Nearest,
    F64_Sqrt,
    F64_Add,
    F64_Sub,
    F64_Mul,
    F64_Div,
    F64_Min,
    F64_Max,
    F64_Copysign,
    I32_Wrap_I64,
    I32_Trunc_F32_S,
    I32_Trunc_F32_U,
    I32_Trunc_F64_S,
    I32_Trunc_F64_U,
    I64_Extend_I32_S,
    I64_Extend_I32_U,
    I64_Trunc_F32_S,
    I64_Trunc_F32_U,
    I64_Trunc_F64_S,
    I64_Trunc_F64_U,
    F32_Convert_I32_S,
    F32_Convert_I32_U,
    F32_Convert_I64_S,
    F32_Convert_I64_U,
    F32_Demote_F64,
    F64_Convert_I32_S,
    F64_Convert_I32_U,
    F64_Convert_I64_S,
    F64_Convert_I64_U,
    F64_Promote_F32,
    I32_Reinterpret_F32,
    I64_Reinterpret_F64,
    F32_Reinterpret_I32,
    F64_Reinterpret_I64,
    I32_Extend8_S,
    I32_Extend16_S,
    I64_Extend8_S,
    I64_Extend16_S,
    I64_Extend32_S,
    Ref_Null,
    Ref_Is_Null,
    Ref_Func,
    I32_Trunc_Sat_F32_S,
    I32_Trunc_Sat_F32_U,
    I32_Trunc_Sat_F64_S,
    I32_Trunc_Sat_F64_U,
    I64_Trunc_Sat_F32_S,
    I64_Trunc_Sat_F32_U,
    I64_Trunc_Sat_F64_S,
    I64_Trunc_Sat_F64_U,
    Memory_Init,
    Data_Drop,
    Memory_Copy,
    Memory_Fill,
    Table_Init,
    Elem_Drop,
    Table_Copy,
    Table_Grow,
    Table_Size,
    Table_Fill,
    V128_Load,
    V128_Load8x8_S,
    V128_Load8x8_U,
    V128_Load16x4_S,
    V128_Load16x4_U,
    V128_Load32x2_S,
    V128_Load32x2_U,
    V128_Load8_Splat,
    V128_Load16_Splat,
    V128_Load32_Splat,
    V128_Load64_Splat,
    V128_Store,
    V128_Const,
    I8x16_Shuffle,
    I8x16_Swizzle,
    I8x16_Splat,
    I16x8_Splat,
    I32x4_Splat,
    I64x2_Splat,
    F32x4_Splat,
    F64x2_Splat,
    I8x16_Extract_Lane_S,
    I8x16_Extract_Lane_U,
    I8x16_Replace_Lane,
    I16x8_Extract_Lane_S,
    I16x8_Extract_Lane_U,
    I16x8_Replace_Lane,
    I32x4_Extract_Lane,
    I32x4_Replace_Lane,
    I64x2_Extract_Lane,
    I64x2_Replace_Lane,
    F32x4_Extract_Lane,
    F32x4_Replace_Lane,
    F64x2_Extract_Lane,
    F64x2_Replace_Lane,
    I8x16_EQ,
    I8x16_NE,
    I8x16_LT_S,
    I8x16_LT_U,
    I8x16_GT_S,
    I8x16_GT_U,
    I8x16_LE_S,
    I8x16_LE_U,
    I8x16_GE_S,
    I8x16_GE_U,
    I16x8_EQ,
    I16x8_NE,
    I16x8_LT_S,
    I16x8_LT_U,
    I16x8_GT_S,
    I16x8_GT_U,
    I16x8_LE_S,
    I16x8_LE_U,
    I16x8_GE_S,
    I16x8_GE_U,
    I32x4_EQ,
    I32x4_NE,
    I32x4_LT_S,
    I32x4_LT_U,
    I32x4_GT_S,
    I32x4_GT_U,
    I32x4_LE_S,
    I32x4_LE_U,
    I32x4_GE_S,
    I32x4_GE_U,
    F32x4_EQ,
    F32x4_NE,
    F32x4_LT,
    F32x4_GT,
    F32x4_LE,
    F32x4_GE,
    F64x2_EQ,
    F64x2_NE,
    F64x2_LT,
    F64x2_GT,
    F64x2_LE,
    F64x2_GE,
    V128_Not,
    V128_And,
    V128_AndNot,
    V128_Or,
    V128_Xor,
    V128_Bitselect,
    V128_AnyTrue,
    V128_Load8_Lane,
    V128_Load16_Lane,
    V128_Load32_Lane,
    V128_Load64_Lane,
    V128_Store8_Lane,
    V128_Store16_Lane,
    V128_Store32_Lane,
    V128_Store64_Lane,
    V128_Load32_Zero,
    V128_Load64_Zero,
    F32x4_Demote_F64x2_Zero,
    F64x2_Promote_Low_F32x4,
    I8x16_Abs,
    I8x16_Neg,
    I8x16_Popcnt,
    I8x16_AllTrue,
    I8x16_Bitmask,
    I8x16_Narrow_I16x8_S,
    I8x16_Narrow_I16x8_U,
    F32x4_Ceil,
    F32x4_Floor,
    F32x4_Trunc,
    F32x4_Nearest,
    I8x16_Shl,
    I8x16_Shr_S,
    I8x16_Shr_U,
    I8x16_Add,
    I8x16_Add_Sat_S,
    I8x16_Add_Sat_U,
    I8x16_Sub,
    I8x16_Sub_Sat_S,
    I8x16_Sub_Sat_U,
    F64x2_Ceil,
    F64x2_Floor,
    I8x16_Min_S,
    I8x16_Min_U,
    I8x16_Max_S,
    I8x16_Max_U,
    F64x2_Trunc,
    I8x16_Avgr_U,
    I16x8_Extadd_Pairwise_I8x16_S,
    I16x8_Extadd_Pairwise_I8x16_U,
    I32x4_Extadd_Pairwise_I16x8_S,
    I32x4_Extadd_Pairwise_I16x8_U,
    I16x8_Abs,
    I16x8_Neg,
    I16x8_Q15mulr_Sat_S,
    I16x8_AllTrue,
    I16x8_Bitmask,
    I16x8_Narrow_I32x4_S,
    I16x8_Narrow_I32x4_U,
    I16x8_Extend_Low_I8x16_S,
    I16x8_Extend_High_I8x16_S,
    I16x8_Extend_Low_I8x16_U,
    I16x8_Extend_High_I8x16_U,
    I16x8_Shl,
    I16x8_Shr_S,
    I16x8_Shr_U,
    I16x8_Add,
    I16x8_Add_Sat_S,
    I16x8_Add_Sat_U,
    I16x8_Sub,
    I16x8_Sub_Sat_S,
    I16x8_Sub_Sat_U,
    F64x2_Nearest,
    I16x8_Mul,
    I16x8_Min_S,
    I16x8_Min_U,
    I16x8_Max_S,
    I16x8_Max_U,
    I16x8_Avgr_U,
    I16x8_Extmul_Low_I8x16_S,
    I16x8_Extmul_High_I8x16_S,
    I16x8_Extmul_Low_I8x16_U,
    I16x8_Extmul_High_I8x16_U,
    I32x4_Abs,
    I32x4_Neg,
    I32x4_AllTrue,
    I32x4_Bitmask,
    I32x4_Extend_Low_I16x8_S,
    I32x4_Extend_High_I16x8_S,
    I32x4_Extend_Low_I16x8_U,
    I32x4_Extend_High_I16x8_U,
    I32x4_Shl,
    I32x4_Shr_S,
    I32x4_Shr_U,
    I32x4_Add,
    I32x4_Sub,
    I32x4_Mul,
    I32x4_Min_S,
    I32x4_Min_U,
    I32x4_Max_S,
    I32x4_Max_U,
    I32x4_Dot_I16x8_S,
    I32x4_Extmul_Low_I16x8_S,
    I32x4_Extmul_High_I16x8_S,
    I32x4_Extmul_Low_I16x8_U,
    I32x4_Extmul_High_I16x8_U,
    I64x2_Abs,
    I64x2_Neg,
    I64x2_AllTrue,
    I64x2_Bitmask,
    I64x2_Extend_Low_I32x4_S,
    I64x2_Extend_High_I32x4_S,
    I64x2_Extend_Low_I32x4_U,
    I64x2_Extend_High_I32x4_U,
    I64x2_Shl,
    I64x2_Shr_S,
    I64x2_Shr_U,
    I64x2_Add,
    I64x2_Sub,
    I64x2_Mul,
    I64x2_EQ,
    I64x2_NE,
    I64x2_LT_S,
    I64x2_GT_S,
    I64x2_LE_S,
    I64x2_GE_S,
    I64x2_Extmul_Low_I32x4_S,
    I64x2_Extmul_High_I32x4_S,
    I64x2_Extmul_Low_I32x4_U,
    I64x2_Extmul_High_I32x4_U,
    F32x4_Abs,
    F32x4_Neg,
    F32x4_Sqrt,
    F32x4_Add,
    F32x4_Sub,
    F32x4_Mul,
    F32x4_Div,
    F32x4_Min,
    F32x4_Max,
    F32x4_PMin,
    F32x4_PMax,
    F64x2_Abs,
    F64x2_Neg,
    F64x2_Sqrt,
    F64x2_Add,
    F64x2_Sub,
    F64x2_Mul,
    F64x2_Div,
    F64x2_Min,
    F64x2_Max,
    F64x2_PMin,
    F64x2_PMax,
    F32x4_Trunc_Sat_F32x4_S,
    F32x4_Trunc_Sat_F32x4_U,
    F32x4_Convert_I32x4_S,
    F32x4_Convert_I32x4_U,
    I32x4_Trunc_Sat_F64x2_S_Zero,
    I32x4_Trunc_Sat_F64x2_U_Zero,
    F64x2_Convert_Low_I32x4_S,
    F64x2_Convert_Low_I32x4_U,

    pub fn beginsBlock(opcode: Opcode) bool {
        return switch (opcode) {
            .Block => true,
            .Loop => true,
            .If => true,
            else => false,
        };
    }

    pub fn isIf(opcode: Opcode) bool {
        return switch (opcode) {
            .If, .IfNoElse => true,
            else => false,
        };
    }
};

pub const WasmOpcode = enum(u16) {
    Unreachable = 0x00,
    Noop = 0x01,
    Block = 0x02,
    Loop = 0x03,
    If = 0x04,
    Else = 0x05,
    End = 0x0B,
    Branch = 0x0C,
    Branch_If = 0x0D,
    Branch_Table = 0x0E,
    Return = 0x0F,
    Call = 0x10,
    Call_Indirect = 0x11,
    Drop = 0x1A,
    Select = 0x1B,
    Select_T = 0x1C,
    Local_Get = 0x20,
    Local_Set = 0x21,
    Local_Tee = 0x22,
    Global_Get = 0x23,
    Global_Set = 0x24,
    Table_Get = 0x25,
    Table_Set = 0x26,
    I32_Load = 0x28,
    I64_Load = 0x29,
    F32_Load = 0x2A,
    F64_Load = 0x2B,
    I32_Load8_S = 0x2C,
    I32_Load8_U = 0x2D,
    I32_Load16_S = 0x2E,
    I32_Load16_U = 0x2F,
    I64_Load8_S = 0x30,
    I64_Load8_U = 0x31,
    I64_Load16_S = 0x32,
    I64_Load16_U = 0x33,
    I64_Load32_S = 0x34,
    I64_Load32_U = 0x35,
    I32_Store = 0x36,
    I64_Store = 0x37,
    F32_Store = 0x38,
    F64_Store = 0x39,
    I32_Store8 = 0x3A,
    I32_Store16 = 0x3B,
    I64_Store8 = 0x3C,
    I64_Store16 = 0x3D,
    I64_Store32 = 0x3E,
    Memory_Size = 0x3F,
    Memory_Grow = 0x40,
    I32_Const = 0x41,
    I64_Const = 0x42,
    F32_Const = 0x43,
    F64_Const = 0x44,
    I32_Eqz = 0x45,
    I32_Eq = 0x46,
    I32_NE = 0x47,
    I32_LT_S = 0x48,
    I32_LT_U = 0x49,
    I32_GT_S = 0x4A,
    I32_GT_U = 0x4B,
    I32_LE_S = 0x4C,
    I32_LE_U = 0x4D,
    I32_GE_S = 0x4E,
    I32_GE_U = 0x4F,
    I64_Eqz = 0x50,
    I64_Eq = 0x51,
    I64_NE = 0x52,
    I64_LT_S = 0x53,
    I64_LT_U = 0x54,
    I64_GT_S = 0x55,
    I64_GT_U = 0x56,
    I64_LE_S = 0x57,
    I64_LE_U = 0x58,
    I64_GE_S = 0x59,
    I64_GE_U = 0x5A,
    F32_EQ = 0x5B,
    F32_NE = 0x5C,
    F32_LT = 0x5D,
    F32_GT = 0x5E,
    F32_LE = 0x5F,
    F32_GE = 0x60,
    F64_EQ = 0x61,
    F64_NE = 0x62,
    F64_LT = 0x63,
    F64_GT = 0x64,
    F64_LE = 0x65,
    F64_GE = 0x66,
    I32_Clz = 0x67,
    I32_Ctz = 0x68,
    I32_Popcnt = 0x69,
    I32_Add = 0x6A,
    I32_Sub = 0x6B,
    I32_Mul = 0x6C,
    I32_Div_S = 0x6D,
    I32_Div_U = 0x6E,
    I32_Rem_S = 0x6F,
    I32_Rem_U = 0x70,
    I32_And = 0x71,
    I32_Or = 0x72,
    I32_Xor = 0x73,
    I32_Shl = 0x74,
    I32_Shr_S = 0x75,
    I32_Shr_U = 0x76,
    I32_Rotl = 0x77,
    I32_Rotr = 0x78,
    I64_Clz = 0x79,
    I64_Ctz = 0x7A,
    I64_Popcnt = 0x7B,
    I64_Add = 0x7C,
    I64_Sub = 0x7D,
    I64_Mul = 0x7E,
    I64_Div_S = 0x7F,
    I64_Div_U = 0x80,
    I64_Rem_S = 0x81,
    I64_Rem_U = 0x82,
    I64_And = 0x83,
    I64_Or = 0x84,
    I64_Xor = 0x85,
    I64_Shl = 0x86,
    I64_Shr_S = 0x87,
    I64_Shr_U = 0x88,
    I64_Rotl = 0x89,
    I64_Rotr = 0x8A,
    F32_Abs = 0x8B,
    F32_Neg = 0x8C,
    F32_Ceil = 0x8D,
    F32_Floor = 0x8E,
    F32_Trunc = 0x8F,
    F32_Nearest = 0x90,
    F32_Sqrt = 0x91,
    F32_Add = 0x92,
    F32_Sub = 0x93,
    F32_Mul = 0x94,
    F32_Div = 0x95,
    F32_Min = 0x96,
    F32_Max = 0x97,
    F32_Copysign = 0x98,
    F64_Abs = 0x99,
    F64_Neg = 0x9A,
    F64_Ceil = 0x9B,
    F64_Floor = 0x9C,
    F64_Trunc = 0x9D,
    F64_Nearest = 0x9E,
    F64_Sqrt = 0x9F,
    F64_Add = 0xA0,
    F64_Sub = 0xA1,
    F64_Mul = 0xA2,
    F64_Div = 0xA3,
    F64_Min = 0xA4,
    F64_Max = 0xA5,
    F64_Copysign = 0xA6,
    I32_Wrap_I64 = 0xA7,
    I32_Trunc_F32_S = 0xA8,
    I32_Trunc_F32_U = 0xA9,
    I32_Trunc_F64_S = 0xAA,
    I32_Trunc_F64_U = 0xAB,
    I64_Extend_I32_S = 0xAC,
    I64_Extend_I32_U = 0xAD,
    I64_Trunc_F32_S = 0xAE,
    I64_Trunc_F32_U = 0xAF,
    I64_Trunc_F64_S = 0xB0,
    I64_Trunc_F64_U = 0xB1,
    F32_Convert_I32_S = 0xB2,
    F32_Convert_I32_U = 0xB3,
    F32_Convert_I64_S = 0xB4,
    F32_Convert_I64_U = 0xB5,
    F32_Demote_F64 = 0xB6,
    F64_Convert_I32_S = 0xB7,
    F64_Convert_I32_U = 0xB8,
    F64_Convert_I64_S = 0xB9,
    F64_Convert_I64_U = 0xBA,
    F64_Promote_F32 = 0xBB,
    I32_Reinterpret_F32 = 0xBC,
    I64_Reinterpret_F64 = 0xBD,
    F32_Reinterpret_I32 = 0xBE,
    F64_Reinterpret_I64 = 0xBF,
    I32_Extend8_S = 0xC0,
    I32_Extend16_S = 0xC1,
    I64_Extend8_S = 0xC2,
    I64_Extend16_S = 0xC3,
    I64_Extend32_S = 0xC4,
    Ref_Null = 0xD0,
    Ref_Is_Null = 0xD1,
    Ref_Func = 0xD2,
    I32_Trunc_Sat_F32_S = 0xFC00,
    I32_Trunc_Sat_F32_U = 0xFC01,
    I32_Trunc_Sat_F64_S = 0xFC02,
    I32_Trunc_Sat_F64_U = 0xFC03,
    I64_Trunc_Sat_F32_S = 0xFC04,
    I64_Trunc_Sat_F32_U = 0xFC05,
    I64_Trunc_Sat_F64_S = 0xFC06,
    I64_Trunc_Sat_F64_U = 0xFC07,
    Memory_Init = 0xFC08,
    Data_Drop = 0xFC09,
    Memory_Copy = 0xFC0A,
    Memory_Fill = 0xFC0B,
    Table_Init = 0xFC0C,
    Elem_Drop = 0xFC0D,
    Table_Copy = 0xFC0E,
    Table_Grow = 0xFC0F,
    Table_Size = 0xFC10,
    Table_Fill = 0xFC11,
    V128_Load = 0xFD00,
    V128_Load8x8_S = 0xFD01,
    V128_Load8x8_U = 0xFD02,
    V128_Load16x4_S = 0xFD03,
    V128_Load16x4_U = 0xFD04,
    V128_Load32x2_S = 0xFD05,
    V128_Load32x2_U = 0xFD06,
    V128_Load8_Splat = 0xFD07,
    V128_Load16_Splat = 0xFD08,
    V128_Load32_Splat = 0xFD09,
    V128_Load64_Splat = 0xFD0A,
    V128_Store = 0xFD0B,
    V128_Const = 0xFD0C,
    I8x16_Shuffle = 0xFD0D,
    I8x16_Swizzle = 0xFD0E,
    I8x16_Splat = 0xFD0F,
    I16x8_Splat = 0xFD10,
    I32x4_Splat = 0xFD11,
    I64x2_Splat = 0xFD12,
    F32x4_Splat = 0xFD13,
    F64x2_Splat = 0xFD14,
    I8x16_Extract_Lane_S = 0xFD15,
    I8x16_Extract_Lane_U = 0xFD16,
    I8x16_Replace_Lane = 0xFD17,
    I16x8_Extract_Lane_S = 0xFD18,
    I16x8_Extract_Lane_U = 0xFD19,
    I16x8_Replace_Lane = 0xFD1A,
    I32x4_Extract_Lane = 0xFD1B,
    I32x4_Replace_Lane = 0xFD1C,
    I64x2_Extract_Lane = 0xFD1D,
    I64x2_Replace_Lane = 0xFD1E,
    F32x4_Extract_Lane = 0xFD1F,
    F32x4_Replace_Lane = 0xFD20,
    F64x2_Extract_Lane = 0xFD21,
    F64x2_Replace_Lane = 0xFD22,
    I8x16_EQ = 0xFD23,
    I8x16_NE = 0xFD24,
    I8x16_LT_S = 0xFD25,
    I8x16_LT_U = 0xFD26,
    I8x16_GT_S = 0xFD27,
    I8x16_GT_U = 0xFD28,
    I8x16_LE_S = 0xFD29,
    I8x16_LE_U = 0xFD2A,
    I8x16_GE_S = 0xFD2B,
    I8x16_GE_U = 0xFD2C,
    I16x8_EQ = 0xFD2D,
    I16x8_NE = 0xFD2E,
    I16x8_LT_S = 0xFD2F,
    I16x8_LT_U = 0xFD30,
    I16x8_GT_S = 0xFD31,
    I16x8_GT_U = 0xFD32,
    I16x8_LE_S = 0xFD33,
    I16x8_LE_U = 0xFD34,
    I16x8_GE_S = 0xFD35,
    I16x8_GE_U = 0xFD36,
    I32x4_EQ = 0xFD37,
    I32x4_NE = 0xFD38,
    I32x4_LT_S = 0xFD39,
    I32x4_LT_U = 0xFD3A,
    I32x4_GT_S = 0xFD3B,
    I32x4_GT_U = 0xFD3C,
    I32x4_LE_S = 0xFD3D,
    I32x4_LE_U = 0xFD3E,
    I32x4_GE_S = 0xFD3F,
    I32x4_GE_U = 0xFD40,
    F32x4_EQ = 0xFD41,
    F32x4_NE = 0xFD42,
    F32x4_LT = 0xFD43,
    F32x4_GT = 0xFD44,
    F32x4_LE = 0xFD45,
    F32x4_GE = 0xFD46,
    F64x2_EQ = 0xFD47,
    F64x2_NE = 0xFD48,
    F64x2_LT = 0xFD49,
    F64x2_GT = 0xFD4A,
    F64x2_LE = 0xFD4B,
    F64x2_GE = 0xFD4C,
    V128_Not = 0xFD4D,
    V128_And = 0xFD4E,
    V128_AndNot = 0xFD4F,
    V128_Or = 0xFD50,
    V128_Xor = 0xFD51,
    V128_Bitselect = 0xFD52,
    V128_AnyTrue = 0xFD53,
    V128_Load8_Lane = 0xFD54,
    V128_Load16_Lane = 0xFD55,
    V128_Load32_Lane = 0xFD56,
    V128_Load64_Lane = 0xFD57,
    V128_Store8_Lane = 0xFD58,
    V128_Store16_Lane = 0xFD59,
    V128_Store32_Lane = 0xFD5A,
    V128_Store64_Lane = 0xFD5B,
    V128_Load32_Zero = 0xFD5C,
    V128_Load64_Zero = 0xFD5D,
    F32x4_Demote_F64x2_Zero = 0xFD5E,
    F64x2_Promote_Low_F32x4 = 0xFD5F,
    I8x16_Abs = 0xFD60,
    I8x16_Neg = 0xFD61,
    I8x16_Popcnt = 0xFD62,
    I8x16_AllTrue = 0xFD63,
    I8x16_Bitmask = 0xFD64,
    II8x16_Narrow_I16x8_S = 0xFD65,
    II8x16_Narrow_I16x8_U = 0xFD66,
    F32x4_Ceil = 0xFD67,
    F32x4_Floor = 0xFD68,
    F32x4_Trunc = 0xFD69,
    F32x4_Nearest = 0xFD6A,
    I8x16_Shl = 0xFD6B,
    I8x16_Shr_S = 0xFD6C,
    I8x16_Shr_U = 0xFD6D,
    I8x16_Add = 0xFD6E,
    I8x16_Add_Sat_S = 0xFD6F,
    I8x16_Add_Sat_U = 0xFD70,
    I8x16_Sub = 0xFD71,
    I8x16_Sub_Sat_S = 0xFD72,
    I8x16_Sub_Sat_U = 0xFD73,
    F64x2_Ceil = 0xFD74,
    F64x2_Floor = 0xFD75,
    I8x16_Min_S = 0xFD76,
    I8x16_Min_U = 0xFD77,
    I8x16_Max_S = 0xFD78,
    I8x16_Max_U = 0xFD79,
    F64x2_Trunc = 0xFD7A,
    I8x16_Avgr_U = 0xFD7B,
    I16x8_Extadd_Pairwise_I8x16_S = 0xFD7C,
    I16x8_Extadd_Pairwise_I8x16_U = 0xFD7D,
    I32x4_Extadd_Pairwise_I16x8_S = 0xFD7E,
    I32x4_Extadd_Pairwise_I16x8_U = 0xFD7F,
    I16x8_Abs = 0xFD80,
    I16x8_Neg = 0xFD81,
    I16x8_Q15mulr_Sat_S = 0xFD82,
    I16x8_AllTrue = 0xFD83,
    I16x8_Bitmask = 0xFD84,
    I16x8_Narrow_I32x4_S = 0xFD85,
    I16x8_Narrow_I32x4_U = 0xFD86,
    I16x8_Extend_Low_I8x16_S = 0xFD87,
    I16x8_Extend_High_I8x16_S = 0xFD88,
    I16x8_Extend_Low_I8x16_U = 0xFD89,
    I16x8_Extend_High_I8x16_U = 0xFD8A,
    I16x8_Shl = 0xFD8B,
    I16x8_Shr_S = 0xFD8C,
    I16x8_Shr_U = 0xFD8D,
    I16x8_Add = 0xFD8E,
    I16x8_Add_Sat_S = 0xFD8F,
    I16x8_Add_Sat_U = 0xFD90,
    I16x8_Sub = 0xFD91,
    I16x8_Sub_Sat_S = 0xFD92,
    I16x8_Sub_Sat_U = 0xFD93,
    F64x2_Nearest = 0xFD94,
    I16x8_Mul = 0xFD95,
    I16x8_Min_S = 0xFD96,
    I16x8_Min_U = 0xFD97,
    I16x8_Max_S = 0xFD98,
    I16x8_Max_U = 0xFD99,
    I16x8_Avgr_U = 0xFD9B,
    I16x8_Extmul_Low_I8x16_S = 0xFD9C,
    I16x8_Extmul_High_I8x16_S = 0xFD9D,
    I16x8_Extmul_Low_I8x16_U = 0xFD9E,
    I16x8_Extmul_High_I8x16_U = 0xFD9F,
    I32x4_Abs = 0xFDA0,
    I32x4_Neg = 0xFDA1,
    I32x4_AllTrue = 0xFDA3,
    I32x4_Bitmask = 0xFDA4,
    I32x4_Extend_Low_I16x8_S = 0xFDA7,
    I32x4_Extend_High_I16x8_S = 0xFDA8,
    I32x4_Extend_Low_I16x8_U = 0xFDA9,
    I32x4_Extend_High_I16x8_U = 0xFDAA,
    I32x4_Shl = 0xFDAB,
    I32x4_Shr_S = 0xFDAC,
    I32x4_Shr_U = 0xFDAD,
    I32x4_Add = 0xFDAE,
    I32x4_Sub = 0xFDB1,
    I32x4_Mul = 0xFDB5,
    I32x4_Min_S = 0xFDB6,
    I32x4_Min_U = 0xFDB7,
    I32x4_Max_S = 0xFDB8,
    I32x4_Max_U = 0xFDB9,
    I32x4_Dot_I16x8_S = 0xFDBA,
    I32x4_Extmul_Low_I16x8_S = 0xFDBC,
    I32x4_Extmul_High_I16x8_S = 0xFDBD,
    I32x4_Extmul_Low_I16x8_U = 0xFDBE,
    I32x4_Extmul_High_I16x8_U = 0xFDBF,
    I64x2_Abs = 0xFDC0,
    I64x2_Neg = 0xFDC1,
    I64x2_AllTrue = 0xFDC3,
    I64x2_Bitmask = 0xFDC4,
    I64x2_Extend_Low_I32x4_S = 0xFDC7,
    I64x2_Extend_High_I32x4_S = 0xFDC8,
    I64x2_Extend_Low_I32x4_U = 0xFDC9,
    I64x2_Extend_High_I32x4_U = 0xFDCA,
    I64x2_Shl = 0xFDCB,
    I64x2_Shr_S = 0xFDCC,
    I64x2_Shr_U = 0xFDCD,
    I64x2_Add = 0xFDCE,
    I64x2_Sub = 0xFDD1,
    I64x2_Mul = 0xFDD5,
    I64x2_EQ = 0xFDD6,
    I64x2_NE = 0xFDD7,
    I64x2_LT_S = 0xFDD8,
    I64x2_GT_S = 0xFDD9,
    I64x2_LE_S = 0xFDDA,
    I64x2_GE_S = 0xFDDB,
    I64x2_Extmul_Low_I32x4_S = 0xFDDC,
    I64x2_Extmul_High_I32x4_S = 0xFDDD,
    I64x2_Extmul_Low_I32x4_U = 0xFDDE,
    I64x2_Extmul_High_I32x4_U = 0xFDDF,
    F32x4_Abs = 0xFDE0,
    F32x4_Neg = 0xFDE1,
    F32x4_Sqrt = 0xFDE3,
    F32x4_Add = 0xFDE4,
    F32x4_Sub = 0xFDE5,
    F32x4_Mul = 0xFDE6,
    F32x4_Div = 0xFDE7,
    F32x4_Min = 0xFDE8,
    F32x4_Max = 0xFDE9,
    F32x4_PMin = 0xFDEA,
    F32x4_PMax = 0xFDEB,
    F64x2_Abs = 0xFDEC,
    F64x2_Neg = 0xFDED,
    F64x2_Sqrt = 0xFDEF,
    F64x2_Add = 0xFDF0,
    F64x2_Sub = 0xFDF1,
    F64x2_Mul = 0xFDF2,
    F64x2_Div = 0xFDF3,
    F64x2_Min = 0xFDF4,
    F64x2_Max = 0xFDF5,
    F64x2_PMin = 0xFDF6,
    F64x2_PMax = 0xFDF7,
    F32x4_Trunc_Sat_F32x4_S = 0xFDF8,
    F32x4_Trunc_Sat_F32x4_U = 0xFDF9,
    F32x4_Convert_I32x4_S = 0xFDFA,
    F32x4_Convert_I32x4_U = 0xFDFB,
    I32x4_Trunc_Sat_F64x2_S_Zero = 0xFDFC,
    I32x4_Trunc_Sat_F64x2_U_Zero = 0xFDFD,
    F64x2_Convert_Low_I32x4_S = 0xFDFE,
    F64x2_Convert_Low_I32x4_U = 0xFDFF,

    pub fn toOpcode(wasm: WasmOpcode) Opcode {
        const opcode_int = @intFromEnum(wasm);
        var opcode: Opcode = undefined;
        if (opcode_int < ConversionTables.wasmOpcodeToOpcodeTable.len) {
            opcode = ConversionTables.wasmOpcodeToOpcodeTable[opcode_int];
        } else if (opcode_int >= 0xFC00 and opcode_int < 0xFCD0) {
            opcode = ConversionTables.wasmFCOpcodeToOpcodeTable[opcode_int - 0xFC00];
        } else {
            opcode = ConversionTables.wasmFDOpcodeToOpcodeTable[opcode_int - 0xFD00];
        }
        std.debug.assert(opcode != .Invalid);
        return opcode;
    }
};

const ConversionTables = struct {
    const wasmOpcodeToOpcodeTable = [_]Opcode{
        Opcode.Unreachable, // 0x00
        Opcode.Noop, // 0x01
        Opcode.Block, // 0x02
        Opcode.Loop, // 0x03
        Opcode.If, // 0x04
        Opcode.Else, // 0x05
        Opcode.Invalid, // 0x06
        Opcode.Invalid, // 0x07
        Opcode.Invalid, // 0x08
        Opcode.Invalid, // 0x09
        Opcode.Invalid, // 0x0A
        Opcode.End, // 0x0B,
        Opcode.Branch, // 0x0C
        Opcode.Branch_If, // 0x0D
        Opcode.Branch_Table, // 0x0E
        Opcode.Return, // 0x0F
        Opcode.Call_Local, // 0x10 (WasmOpcode.Call)
        Opcode.Call_Indirect, // 0x11
        Opcode.Invalid, // 0x12
        Opcode.Invalid, // 0x13
        Opcode.Invalid, // 0x14
        Opcode.Invalid, // 0x15
        Opcode.Invalid, // 0x16
        Opcode.Invalid, // 0x17
        Opcode.Invalid, // 0x18
        Opcode.Invalid, // 0x19
        Opcode.Drop, // 0x1A
        Opcode.Select, // 0x1B
        Opcode.Select_T, // 0x1C
        Opcode.Invalid, // 0x1D
        Opcode.Invalid, // 0x1E
        Opcode.Invalid, // 0x1F
        Opcode.Local_Get, // 0x20
        Opcode.Local_Set, // 0x21
        Opcode.Local_Tee, // 0x22
        Opcode.Global_Get, // 0x23
        Opcode.Global_Set, // 0x24
        Opcode.Table_Get, // 0x25
        Opcode.Table_Set, // 0x26
        Opcode.Invalid, // 0x27
        Opcode.I32_Load, // 0x28
        Opcode.I64_Load, // 0x29
        Opcode.F32_Load, // 0x2A
        Opcode.F64_Load, // 0x2B
        Opcode.I32_Load8_S, // 0x2C
        Opcode.I32_Load8_U, // 0x2D
        Opcode.I32_Load16_S, // 0x2E
        Opcode.I32_Load16_U, // 0x2F
        Opcode.I64_Load8_S, // 0x30
        Opcode.I64_Load8_U, // 0x31
        Opcode.I64_Load16_S, // 0x32
        Opcode.I64_Load16_U, // 0x33
        Opcode.I64_Load32_S, // 0x34
        Opcode.I64_Load32_U, // 0x35
        Opcode.I32_Store, // 0x36
        Opcode.I64_Store, // 0x37
        Opcode.F32_Store, // 0x38
        Opcode.F64_Store, // 0x39
        Opcode.I32_Store8, // 0x3A
        Opcode.I32_Store16, // 0x3B
        Opcode.I64_Store8, // 0x3C
        Opcode.I64_Store16, // 0x3D
        Opcode.I64_Store32, // 0x3E
        Opcode.Memory_Size, // 0x3F
        Opcode.Memory_Grow, // 0x40
        Opcode.I32_Const, // 0x41
        Opcode.I64_Const, // 0x42
        Opcode.F32_Const, // 0x43
        Opcode.F64_Const, // 0x44
        Opcode.I32_Eqz, // 0x45
        Opcode.I32_Eq, // 0x46
        Opcode.I32_NE, // 0x47
        Opcode.I32_LT_S, // 0x48
        Opcode.I32_LT_U, // 0x49
        Opcode.I32_GT_S, // 0x4A
        Opcode.I32_GT_U, // 0x4B
        Opcode.I32_LE_S, // 0x4C
        Opcode.I32_LE_U, // 0x4D
        Opcode.I32_GE_S, // 0x4E
        Opcode.I32_GE_U, // 0x4F
        Opcode.I64_Eqz, // 0x50
        Opcode.I64_Eq, // 0x51
        Opcode.I64_NE, // 0x52
        Opcode.I64_LT_S, // 0x53
        Opcode.I64_LT_U, // 0x54
        Opcode.I64_GT_S, // 0x55
        Opcode.I64_GT_U, // 0x56
        Opcode.I64_LE_S, // 0x57
        Opcode.I64_LE_U, // 0x58
        Opcode.I64_GE_S, // 0x59
        Opcode.I64_GE_U, // 0x5A
        Opcode.F32_EQ, // 0x5B
        Opcode.F32_NE, // 0x5C
        Opcode.F32_LT, // 0x5D
        Opcode.F32_GT, // 0x5E
        Opcode.F32_LE, // 0x5F
        Opcode.F32_GE, // 0x60
        Opcode.F64_EQ, // 0x61
        Opcode.F64_NE, // 0x62
        Opcode.F64_LT, // 0x63
        Opcode.F64_GT, // 0x64
        Opcode.F64_LE, // 0x65
        Opcode.F64_GE, // 0x66
        Opcode.I32_Clz, // 0x67
        Opcode.I32_Ctz, // 0x68
        Opcode.I32_Popcnt, // 0x69
        Opcode.I32_Add, // 0x6A
        Opcode.I32_Sub, // 0x6B
        Opcode.I32_Mul, // 0x6C
        Opcode.I32_Div_S, // 0x6D
        Opcode.I32_Div_U, // 0x6E
        Opcode.I32_Rem_S, // 0x6F
        Opcode.I32_Rem_U, // 0x70
        Opcode.I32_And, // 0x71
        Opcode.I32_Or, // 0x72
        Opcode.I32_Xor, // 0x73
        Opcode.I32_Shl, // 0x74
        Opcode.I32_Shr_S, // 0x75
        Opcode.I32_Shr_U, // 0x76
        Opcode.I32_Rotl, // 0x77
        Opcode.I32_Rotr, // 0x78
        Opcode.I64_Clz, // 0x79
        Opcode.I64_Ctz, // 0x7A
        Opcode.I64_Popcnt, // 0x7B
        Opcode.I64_Add, // 0x7C
        Opcode.I64_Sub, // 0x7D
        Opcode.I64_Mul, // 0x7E
        Opcode.I64_Div_S, // 0x7F
        Opcode.I64_Div_U, // 0x80
        Opcode.I64_Rem_S, // 0x81
        Opcode.I64_Rem_U, // 0x82
        Opcode.I64_And, // 0x83
        Opcode.I64_Or, // 0x84
        Opcode.I64_Xor, // 0x85
        Opcode.I64_Shl, // 0x86
        Opcode.I64_Shr_S, // 0x87
        Opcode.I64_Shr_U, // 0x88
        Opcode.I64_Rotl, // 0x89
        Opcode.I64_Rotr, // 0x8A
        Opcode.F32_Abs, // 0x8B
        Opcode.F32_Neg, // 0x8C
        Opcode.F32_Ceil, // 0x8D
        Opcode.F32_Floor, // 0x8E
        Opcode.F32_Trunc, // 0x8F
        Opcode.F32_Nearest, // 0x90
        Opcode.F32_Sqrt, // 0x91
        Opcode.F32_Add, // 0x92
        Opcode.F32_Sub, // 0x93
        Opcode.F32_Mul, // 0x94
        Opcode.F32_Div, // 0x95
        Opcode.F32_Min, // 0x96
        Opcode.F32_Max, // 0x97
        Opcode.F32_Copysign, // 0x98
        Opcode.F64_Abs, // 0x99
        Opcode.F64_Neg, // 0x9A
        Opcode.F64_Ceil, // 0x9B
        Opcode.F64_Floor, // 0x9C
        Opcode.F64_Trunc, // 0x9D
        Opcode.F64_Nearest, // 0x9E
        Opcode.F64_Sqrt, // 0x9F
        Opcode.F64_Add, // 0xA0
        Opcode.F64_Sub, // 0xA1
        Opcode.F64_Mul, // 0xA2
        Opcode.F64_Div, // 0xA3
        Opcode.F64_Min, // 0xA4
        Opcode.F64_Max, // 0xA5
        Opcode.F64_Copysign, // 0xA6
        Opcode.I32_Wrap_I64, // 0xA7
        Opcode.I32_Trunc_F32_S, // 0xA8
        Opcode.I32_Trunc_F32_U, // 0xA9
        Opcode.I32_Trunc_F64_S, // 0xAA
        Opcode.I32_Trunc_F64_U, // 0xAB
        Opcode.I64_Extend_I32_S, // 0xAC
        Opcode.I64_Extend_I32_U, // 0xAD
        Opcode.I64_Trunc_F32_S, // 0xAE
        Opcode.I64_Trunc_F32_U, // 0xAF
        Opcode.I64_Trunc_F64_S, // 0xB0
        Opcode.I64_Trunc_F64_U, // 0xB1
        Opcode.F32_Convert_I32_S, // 0xB2
        Opcode.F32_Convert_I32_U, // 0xB3
        Opcode.F32_Convert_I64_S, // 0xB4
        Opcode.F32_Convert_I64_U, // 0xB5
        Opcode.F32_Demote_F64, // 0xB6
        Opcode.F64_Convert_I32_S, // 0xB7
        Opcode.F64_Convert_I32_U, // 0xB8
        Opcode.F64_Convert_I64_S, // 0xB9
        Opcode.F64_Convert_I64_U, // 0xBA
        Opcode.F64_Promote_F32, // 0xBB
        Opcode.I32_Reinterpret_F32, // 0xBC
        Opcode.I64_Reinterpret_F64, // 0xBD
        Opcode.F32_Reinterpret_I32, // 0xBE
        Opcode.F64_Reinterpret_I64, // 0xBF
        Opcode.I32_Extend8_S, // 0xC0
        Opcode.I32_Extend16_S, // 0xC1
        Opcode.I64_Extend8_S, // 0xC2
        Opcode.I64_Extend16_S, // 0xC3
        Opcode.I64_Extend32_S, // 0xC4
        Opcode.Invalid, // 0xC5
        Opcode.Invalid, // 0xC6
        Opcode.Invalid, // 0xC7
        Opcode.Invalid, // 0xC8
        Opcode.Invalid, // 0xC9
        Opcode.Invalid, // 0xCA
        Opcode.Invalid, // 0xCB
        Opcode.Invalid, // 0xCC
        Opcode.Invalid, // 0xCD
        Opcode.Invalid, // 0xCE
        Opcode.Invalid, // 0xCF
        Opcode.Ref_Null, // 0xD0
        Opcode.Ref_Is_Null, // 0xD1
        Opcode.Ref_Func, // 0xD2
    };

    const wasmFCOpcodeToOpcodeTable = [_]Opcode{
        Opcode.I32_Trunc_Sat_F32_S, // 0xFC00
        Opcode.I32_Trunc_Sat_F32_U, // 0xFC01
        Opcode.I32_Trunc_Sat_F64_S, // 0xFC02
        Opcode.I32_Trunc_Sat_F64_U, // 0xFC03
        Opcode.I64_Trunc_Sat_F32_S, // 0xFC04
        Opcode.I64_Trunc_Sat_F32_U, // 0xFC05
        Opcode.I64_Trunc_Sat_F64_S, // 0xFC06
        Opcode.I64_Trunc_Sat_F64_U, // 0xFC07
        Opcode.Memory_Init, // 0xFC08
        Opcode.Data_Drop, // 0xFC09
        Opcode.Memory_Copy, // 0xFC0A
        Opcode.Memory_Fill, // 0xFC0B
        Opcode.Table_Init, // 0xFC0C
        Opcode.Elem_Drop, // 0xFC0D
        Opcode.Table_Copy, // 0xFC0E
        Opcode.Table_Grow, // 0xFC0F
        Opcode.Table_Size, // 0xFC10
        Opcode.Table_Fill, // 0xFC11
    };

    const wasmFDOpcodeToOpcodeTable = [_]Opcode{
        Opcode.V128_Load, // 0xFD00
        Opcode.V128_Load8x8_S, // 0xFD01
        Opcode.V128_Load8x8_U, // 0xFD02
        Opcode.V128_Load16x4_S, // 0xFD03
        Opcode.V128_Load16x4_U, // 0xFD04
        Opcode.V128_Load32x2_S, // 0xFD05
        Opcode.V128_Load32x2_U, // 0xFD06
        Opcode.V128_Load8_Splat, // 0xFD07
        Opcode.V128_Load16_Splat, // 0xFD08
        Opcode.V128_Load32_Splat, // 0xFD09
        Opcode.V128_Load64_Splat, // 0xFD0A
        Opcode.V128_Store, // 0xFD0B
        Opcode.V128_Const, // 0xFD0C
        Opcode.I8x16_Shuffle, // 0xFD0D
        Opcode.I8x16_Swizzle, // 0xFD0E
        Opcode.I8x16_Splat, // 0xFD0F
        Opcode.I16x8_Splat, // 0xFD10
        Opcode.I32x4_Splat, // 0xFD11
        Opcode.I64x2_Splat, // 0xFD12
        Opcode.F32x4_Splat, // 0xFD13
        Opcode.F64x2_Splat, // 0xFD14
        Opcode.I8x16_Extract_Lane_S, // 0xFD15
        Opcode.I8x16_Extract_Lane_U, // 0xFD16
        Opcode.I8x16_Replace_Lane, // 0xFD17
        Opcode.I16x8_Extract_Lane_S, // 0xFD18
        Opcode.I16x8_Extract_Lane_U, // 0xFD19
        Opcode.I16x8_Replace_Lane, // 0xFD1A
        Opcode.I32x4_Extract_Lane, // 0xFD1B
        Opcode.I32x4_Replace_Lane, // 0xFD1C
        Opcode.I64x2_Extract_Lane, // 0xFD1D
        Opcode.I64x2_Replace_Lane, // 0xFD1E
        Opcode.F32x4_Extract_Lane, // 0xFD1F
        Opcode.F32x4_Replace_Lane, // 0xFD20
        Opcode.F64x2_Extract_Lane, // 0xFD21
        Opcode.F64x2_Replace_Lane, // 0xFD22
        Opcode.I8x16_EQ, // 0xFD23
        Opcode.I8x16_NE, // 0xFD24
        Opcode.I8x16_LT_S, // 0xFD25
        Opcode.I8x16_LT_U, // 0xFD26
        Opcode.I8x16_GT_S, // 0xFD27
        Opcode.I8x16_GT_U, // 0xFD28
        Opcode.I8x16_LE_S, // 0xFD29
        Opcode.I8x16_LE_U, // 0xFD2A
        Opcode.I8x16_GE_S, // 0xFD2B
        Opcode.I8x16_GE_U, // 0xFD2C
        Opcode.I16x8_EQ, // 0xFD2D
        Opcode.I16x8_NE, // 0xFD2E
        Opcode.I16x8_LT_S, // 0xFD2F
        Opcode.I16x8_LT_U, // 0xFD30
        Opcode.I16x8_GT_S, // 0xFD31
        Opcode.I16x8_GT_U, // 0xFD32
        Opcode.I16x8_LE_S, // 0xFD33
        Opcode.I16x8_LE_U, // 0xFD34
        Opcode.I16x8_GE_S, // 0xFD35
        Opcode.I16x8_GE_U, // 0xFD36
        Opcode.I32x4_EQ, // 0xFD37
        Opcode.I32x4_NE, // 0xFD38
        Opcode.I32x4_LT_S, // 0xFD39
        Opcode.I32x4_LT_U, // 0xFD3A
        Opcode.I32x4_GT_S, // 0xFD3B
        Opcode.I32x4_GT_U, // 0xFD3C
        Opcode.I32x4_LE_S, // 0xFD3D
        Opcode.I32x4_LE_U, // 0xFD3E
        Opcode.I32x4_GE_S, // 0xFD3F
        Opcode.I32x4_GE_U, // 0xFD40
        Opcode.F32x4_EQ, // 0xFD41
        Opcode.F32x4_NE, // 0xFD42
        Opcode.F32x4_LT, // 0xFD43
        Opcode.F32x4_GT, // 0xFD44
        Opcode.F32x4_LE, // 0xFD45
        Opcode.F32x4_GE, // 0xFD46
        Opcode.F64x2_EQ, // 0xFD47
        Opcode.F64x2_NE, // 0xFD48
        Opcode.F64x2_LT, // 0xFD49
        Opcode.F64x2_GT, // 0xFD4A
        Opcode.F64x2_LE, // 0xFD4B
        Opcode.F64x2_GE, // 0xFD4C
        Opcode.V128_Not, // 0xFD4D
        Opcode.V128_And, // 0xFD4E
        Opcode.V128_AndNot, // 0xFD4F
        Opcode.V128_Or, // 0xFD50
        Opcode.V128_Xor, // 0xFD51
        Opcode.V128_Bitselect, // 0xFD52
        Opcode.V128_AnyTrue, // 0xFD53
        Opcode.V128_Load8_Lane, // 0xFD54
        Opcode.V128_Load16_Lane, // 0xFD55
        Opcode.V128_Load32_Lane, // 0xFD56
        Opcode.V128_Load64_Lane, // 0xFD57
        Opcode.V128_Store8_Lane, // 0xFD58
        Opcode.V128_Store16_Lane, // 0xFD59
        Opcode.V128_Store32_Lane, // 0xFD5A
        Opcode.V128_Store64_Lane, // 0xFD5B
        Opcode.V128_Load32_Zero, // 0xFD5C
        Opcode.V128_Load64_Zero, // 0xFD5D
        Opcode.F32x4_Demote_F64x2_Zero, // 0xFD5E
        Opcode.F64x2_Promote_Low_F32x4, // 0xFD5F
        Opcode.I8x16_Abs, // 0xFD60
        Opcode.I8x16_Neg, // 0xFD61
        Opcode.I8x16_Popcnt, // 0xFD62
        Opcode.I8x16_AllTrue, // 0xFD63
        Opcode.I8x16_Bitmask, // 0xFD64
        Opcode.I8x16_Narrow_I16x8_S, // 0xFD65
        Opcode.I8x16_Narrow_I16x8_U, // 0xFD66
        Opcode.F32x4_Ceil, // 0xFD67
        Opcode.F32x4_Floor, // 0xFD68
        Opcode.F32x4_Trunc, // 0xFD69
        Opcode.F32x4_Nearest, // 0xFD6A
        Opcode.I8x16_Shl, // 0xFD6B
        Opcode.I8x16_Shr_S, // 0xFD6C
        Opcode.I8x16_Shr_U, // 0xFD6D
        Opcode.I8x16_Add, // 0xFD6E
        Opcode.I8x16_Add_Sat_S, // 0xFD6F
        Opcode.I8x16_Add_Sat_U, // 0xFD70
        Opcode.I8x16_Sub, // 0xFD71
        Opcode.I8x16_Sub_Sat_S, // 0xFD72
        Opcode.I8x16_Sub_Sat_U, // 0xFD73
        Opcode.F64x2_Ceil, // 0xFD74
        Opcode.F64x2_Floor, // 0xFD75
        Opcode.I8x16_Min_S, // 0xFD76
        Opcode.I8x16_Min_U, // 0xFD77
        Opcode.I8x16_Max_S, // 0xFD78
        Opcode.I8x16_Max_U, // 0xFD79
        Opcode.F64x2_Trunc, // 0xFD7A
        Opcode.I8x16_Avgr_U, // 0xFD7B
        Opcode.I16x8_Extadd_Pairwise_I8x16_S, // 0xFD7C
        Opcode.I16x8_Extadd_Pairwise_I8x16_U, // 0xFD7D
        Opcode.I32x4_Extadd_Pairwise_I16x8_S, // 0xFD7E
        Opcode.I32x4_Extadd_Pairwise_I16x8_U, // 0xFD7F
        Opcode.I16x8_Abs, // 0xFD80
        Opcode.I16x8_Neg, // 0xFD81
        Opcode.I16x8_Q15mulr_Sat_S, // 0xFD82
        Opcode.I16x8_AllTrue, // 0xFD83
        Opcode.I16x8_Bitmask, // 0xFD84
        Opcode.I16x8_Narrow_I32x4_S, // 0xFD85
        Opcode.I16x8_Narrow_I32x4_U, // 0xFD86
        Opcode.I16x8_Extend_Low_I8x16_S, // 0xFD87
        Opcode.I16x8_Extend_High_I8x16_S, // 0xFD88
        Opcode.I16x8_Extend_Low_I8x16_U, // 0xFD89
        Opcode.I16x8_Extend_High_I8x16_U, // 0xFD8A
        Opcode.I16x8_Shl, // 0xFD8B
        Opcode.I16x8_Shr_S, // 0xFD8C
        Opcode.I16x8_Shr_U, // 0xFD8D
        Opcode.I16x8_Add, // 0xFD8E
        Opcode.I16x8_Add_Sat_S, // 0xFD8F
        Opcode.I16x8_Add_Sat_U, // 0xFD90
        Opcode.I16x8_Sub, // 0xFD91
        Opcode.I16x8_Sub_Sat_S, // 0xFD92
        Opcode.I16x8_Sub_Sat_U, // 0xFD93
        Opcode.F64x2_Nearest, // 0xFD94
        Opcode.I16x8_Mul, // 0xFD95
        Opcode.I16x8_Min_S, // 0xFD96
        Opcode.I16x8_Min_U, // 0xFD97
        Opcode.I16x8_Max_S, // 0xFD98
        Opcode.I16x8_Max_U, // 0xFD99
        Opcode.Invalid, // 0xFD9A
        Opcode.I16x8_Avgr_U, // 0xFD9B
        Opcode.I16x8_Extmul_Low_I8x16_S, // 0xFD9C
        Opcode.I16x8_Extmul_High_I8x16_S, // 0xFD9D
        Opcode.I16x8_Extmul_Low_I8x16_U, // 0xFD9E
        Opcode.I16x8_Extmul_High_I8x16_U, // 0xFD9F
        Opcode.I32x4_Abs, // 0xFDA0
        Opcode.I32x4_Neg, // 0xFDA1
        Opcode.Invalid, // 0xFDA2
        Opcode.I32x4_AllTrue, // 0xFDA3
        Opcode.I32x4_Bitmask, // 0xFDA4
        Opcode.Invalid, // 0xFDA5
        Opcode.Invalid, // 0xFDA6
        Opcode.I32x4_Extend_Low_I16x8_S, // 0xFDA7
        Opcode.I32x4_Extend_High_I16x8_S, // 0xFDA8
        Opcode.I32x4_Extend_Low_I16x8_U, // 0xFDA9
        Opcode.I32x4_Extend_High_I16x8_U, // 0xFDAA
        Opcode.I32x4_Shl, // 0xFDAB
        Opcode.I32x4_Shr_S, // 0xFDAC
        Opcode.I32x4_Shr_U, // 0xFDAD
        Opcode.I32x4_Add, // 0xFDAE
        Opcode.Invalid, // 0xFDAF
        Opcode.Invalid, // 0xFDB0
        Opcode.I32x4_Sub, // 0xFDB1
        Opcode.Invalid, // 0xFDB2
        Opcode.Invalid, // 0xFDB3
        Opcode.Invalid, // 0xFDB4
        Opcode.I32x4_Mul, // 0xFDB5
        Opcode.I32x4_Min_S, // 0xFDB6
        Opcode.I32x4_Min_U, // 0xFDB7
        Opcode.I32x4_Max_S, // 0xFDB8
        Opcode.I32x4_Max_U, // 0xFDB9
        Opcode.I32x4_Dot_I16x8_S, // 0xFDBA
        Opcode.Invalid, // 0xFDBB
        Opcode.I32x4_Extmul_Low_I16x8_S, // 0xFDBC
        Opcode.I32x4_Extmul_High_I16x8_S, // 0xFDBD
        Opcode.I32x4_Extmul_Low_I16x8_U, // 0xFDBE
        Opcode.I32x4_Extmul_High_I16x8_U, // 0xFDBF
        Opcode.I64x2_Abs, // 0xFDC0
        Opcode.I64x2_Neg, // 0xFDC1
        Opcode.Invalid, // 0xFDC2
        Opcode.I64x2_AllTrue, // 0xFDC3
        Opcode.I64x2_Bitmask, // 0xFDC4
        Opcode.Invalid, // 0xFDC5
        Opcode.Invalid, // 0xFDC6
        Opcode.I64x2_Extend_Low_I32x4_S, // 0xFDC7
        Opcode.I64x2_Extend_High_I32x4_S, // 0xFDC8
        Opcode.I64x2_Extend_Low_I32x4_U, // 0xFDC9
        Opcode.I64x2_Extend_High_I32x4_U, // 0xFDCA
        Opcode.I64x2_Shl, // 0xFDCB
        Opcode.I64x2_Shr_S, // 0xFDCC
        Opcode.I64x2_Shr_U, // 0xFDCD
        Opcode.I64x2_Add, // 0xFDCE
        Opcode.Invalid, // 0xFDCF
        Opcode.Invalid, // 0xFDD0
        Opcode.I64x2_Sub, // 0xFDD1
        Opcode.Invalid, // 0xFDD2
        Opcode.Invalid, // 0xFDD3
        Opcode.Invalid, // 0xFDD4
        Opcode.I64x2_Mul, // 0xFDD5
        Opcode.I64x2_EQ, // 0xFDD6
        Opcode.I64x2_NE, // 0xFDD7
        Opcode.I64x2_LT_S, // 0xFDD8
        Opcode.I64x2_GT_S, // 0xFDD9
        Opcode.I64x2_LE_S, // 0xFDDA
        Opcode.I64x2_GE_S, // 0xFDDB
        Opcode.I64x2_Extmul_Low_I32x4_S, // 0xFDDC
        Opcode.I64x2_Extmul_High_I32x4_S, // 0xFDDD
        Opcode.I64x2_Extmul_Low_I32x4_U, // 0xFDDE
        Opcode.I64x2_Extmul_High_I32x4_U, // 0xFDDF
        Opcode.F32x4_Abs, // 0xFDE0
        Opcode.F32x4_Neg, // 0xFDE1
        Opcode.Invalid, // 0xFDE2
        Opcode.F32x4_Sqrt, // 0xFDE3
        Opcode.F32x4_Add, // 0xFDE4
        Opcode.F32x4_Sub, // 0xFDE5
        Opcode.F32x4_Mul, // 0xFDE6
        Opcode.F32x4_Div, // 0xFDE7
        Opcode.F32x4_Min, // 0xFDE8
        Opcode.F32x4_Max, // 0xFDE9
        Opcode.F32x4_PMin, // 0xFDEA
        Opcode.F32x4_PMax, // 0xFDEB
        Opcode.F64x2_Abs, // 0xFDEC
        Opcode.F64x2_Neg, // 0xFDED
        Opcode.Invalid, // 0xFDEE
        Opcode.F64x2_Sqrt, // 0xFDEF
        Opcode.F64x2_Add, // 0xFDF0
        Opcode.F64x2_Sub, // 0xFDF1
        Opcode.F64x2_Mul, // 0xFDF2
        Opcode.F64x2_Div, // 0xFDF3
        Opcode.F64x2_Min, // 0xFDF4
        Opcode.F64x2_Max, // 0xFDF5
        Opcode.F64x2_PMin, // 0xFDF6
        Opcode.F64x2_PMax, // 0xFDF7
        Opcode.F32x4_Trunc_Sat_F32x4_S, // 0xFDF8
        Opcode.F32x4_Trunc_Sat_F32x4_U, // 0xFDF9
        Opcode.F32x4_Convert_I32x4_S, // 0xFDFA
        Opcode.F32x4_Convert_I32x4_U, // 0xFDFB
        Opcode.I32x4_Trunc_Sat_F64x2_S_Zero, // 0xFDFC
        Opcode.I32x4_Trunc_Sat_F64x2_U_Zero, // 0xFDFD
        Opcode.F64x2_Convert_Low_I32x4_S, // 0xFDFE
        Opcode.F64x2_Convert_Low_I32x4_U, // 0xFDFF
    };
};
