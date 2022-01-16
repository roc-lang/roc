use roc_builtins::bitcode::{self, FloatWidth, IntWidth};
use roc_module::low_level::{LowLevel, LowLevel::*};
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout};
use roc_reporting::internal_error;

use crate::layout::{
    StackMemoryFormat::{self, *},
    WasmLayout,
};
use crate::storage::{Storage, StoredValue};
use crate::wasm_module::{
    Align, CodeBuilder,
    ValueType::{self, *},
};

#[derive(Debug)]
pub enum LowlevelBuildResult {
    Done,
    BuiltinCall(&'static str),
    NotImplemented,
}

/// Number types used for Wasm code gen
/// Unlike other enums, this contains no details about layout or storage.
/// Its purpose is to help simplify the arms of the main lowlevel `match` below.
///
/// Note: Wasm I32 is used for Roc I8, I16, I32, U8, U16, and U32, since it's
/// the smallest integer supported in the Wasm instruction set.
/// We may choose different instructions for signed and unsigned integers,
/// but they share the same Wasm value type.
enum CodeGenNumber {
    I32,     // Supported in Wasm instruction set
    I64,     // Supported in Wasm instruction set
    F32,     // Supported in Wasm instruction set
    F64,     // Supported in Wasm instruction set
    I128,    // Bytes in memory, needs Zig builtins
    F128,    // Bytes in memory, needs Zig builtins
    Decimal, // Bytes in memory, needs Zig builtins
}

impl From<Layout<'_>> for CodeGenNumber {
    fn from(layout: Layout) -> CodeGenNumber {
        use CodeGenNumber::*;

        let not_num_error =
            || internal_error!("Tried to perform a Num low-level operation on {:?}", layout);
        match layout {
            Layout::Builtin(builtin) => match builtin {
                Builtin::Int(int_width) => match int_width {
                    IntWidth::U8 => I32,
                    IntWidth::U16 => I32,
                    IntWidth::U32 => I32,
                    IntWidth::U64 => I64,
                    IntWidth::U128 => I128,
                    IntWidth::I8 => I32,
                    IntWidth::I16 => I32,
                    IntWidth::I32 => I32,
                    IntWidth::I64 => I64,
                    IntWidth::I128 => I128,
                },
                Builtin::Float(float_width) => match float_width {
                    FloatWidth::F32 => F32,
                    FloatWidth::F64 => F64,
                    FloatWidth::F128 => F128,
                },
                Builtin::Decimal => Decimal,
                _ => not_num_error(),
            },
            _ => not_num_error(),
        }
    }
}

impl From<ValueType> for CodeGenNumber {
    fn from(value_type: ValueType) -> CodeGenNumber {
        match value_type {
            ValueType::I32 => CodeGenNumber::I32,
            ValueType::I64 => CodeGenNumber::I64,
            ValueType::F32 => CodeGenNumber::F32,
            ValueType::F64 => CodeGenNumber::F64,
        }
    }
}

impl From<StackMemoryFormat> for CodeGenNumber {
    fn from(format: StackMemoryFormat) -> CodeGenNumber {
        match format {
            StackMemoryFormat::Int128 => CodeGenNumber::I128,
            StackMemoryFormat::Float128 => CodeGenNumber::F128,
            StackMemoryFormat::Decimal => CodeGenNumber::Decimal,
            StackMemoryFormat::DataStructure => {
                internal_error!("Tried to perform a Num low-level operation on a data structure")
            }
        }
    }
}

impl From<WasmLayout> for CodeGenNumber {
    fn from(wasm_layout: WasmLayout) -> CodeGenNumber {
        match wasm_layout {
            WasmLayout::Primitive(value_type, _) => CodeGenNumber::from(value_type),
            WasmLayout::StackMemory { format, .. } => CodeGenNumber::from(format),
        }
    }
}

impl From<StoredValue> for CodeGenNumber {
    fn from(stored: StoredValue) -> CodeGenNumber {
        match stored {
            StoredValue::VirtualMachineStack { value_type, .. } => CodeGenNumber::from(value_type),
            StoredValue::Local { value_type, .. } => CodeGenNumber::from(value_type),
            StoredValue::StackMemory { format, .. } => CodeGenNumber::from(format),
        }
    }
}

pub fn dispatch_low_level<'a>(
    code_builder: &mut CodeBuilder<'a>,
    storage: &mut Storage<'a>,
    lowlevel: LowLevel,
    args: &[Symbol],
    ret_layout: &WasmLayout,
    mono_layout: &Layout<'a>,
) -> LowlevelBuildResult {
    use LowlevelBuildResult::*;

    let panic_ret_type =
        || internal_error!("Invalid return layout for {:?}: {:?}", lowlevel, ret_layout);

    match lowlevel {
        // Str
        StrConcat => return BuiltinCall(bitcode::STR_CONCAT),
        StrJoinWith => return BuiltinCall(bitcode::STR_JOIN_WITH),
        StrIsEmpty => {
            code_builder.i64_const(i64::MIN);
            code_builder.i64_eq();
        }
        StrStartsWith => return BuiltinCall(bitcode::STR_STARTS_WITH),
        StrStartsWithCodePt => return BuiltinCall(bitcode::STR_STARTS_WITH_CODE_PT),
        StrEndsWith => return BuiltinCall(bitcode::STR_ENDS_WITH),
        StrSplit => {
            // LLVM implementation (build_str.rs) does the following
            // 1. Call bitcode::STR_COUNT_SEGMENTS
            // 2. Allocate a `List Str`
            // 3. Call bitcode::STR_STR_SPLIT_IN_PLACE
            // 4. Write the elements and length of the List
            // To do this here, we need full access to WasmBackend, or we could make a Zig wrapper
            return NotImplemented;
        }
        StrCountGraphemes => return BuiltinCall(bitcode::STR_COUNT_GRAPEHEME_CLUSTERS),
        StrToNum => {
            let number_layout = match mono_layout {
                Layout::Struct(fields) => fields[0],
                _ => internal_error!("Unexpected mono layout {:?} for StrToNum", mono_layout),
            };
            // match on the return layout to figure out which zig builtin we need
            let intrinsic = match number_layout {
                Layout::Builtin(Builtin::Int(int_width)) => &bitcode::STR_TO_INT[int_width],
                Layout::Builtin(Builtin::Float(float_width)) => &bitcode::STR_TO_FLOAT[float_width],
                Layout::Builtin(Builtin::Decimal) => bitcode::DEC_FROM_STR,
                rest => internal_error!("Unexpected builtin {:?} for StrToNum", rest),
            };

            return BuiltinCall(intrinsic);
        }
        StrFromInt => {
            // This does not get exposed in user space. We switched to NumToStr instead.
            // We can probably just leave this as NotImplemented. We may want remove this LowLevel.
            // see: https://github.com/rtfeldman/roc/pull/2108
            return NotImplemented;
        }
        StrFromFloat => {
            // linker errors for __ashlti3, __fixunsdfti, __multi3, __udivti3, __umodti3
            // https://gcc.gnu.org/onlinedocs/gccint/Integer-library-routines.html
            // https://gcc.gnu.org/onlinedocs/gccint/Soft-float-library-routines.html
            return NotImplemented;
        }
        StrFromUtf8 => return BuiltinCall(bitcode::STR_FROM_UTF8),
        StrTrimLeft => return BuiltinCall(bitcode::STR_TRIM_LEFT),
        StrTrimRight => return BuiltinCall(bitcode::STR_TRIM_RIGHT),
        StrFromUtf8Range => return BuiltinCall(bitcode::STR_FROM_UTF8_RANGE),
        StrToUtf8 => return BuiltinCall(bitcode::STR_TO_UTF8),
        StrRepeat => return BuiltinCall(bitcode::STR_REPEAT),
        StrTrim => return BuiltinCall(bitcode::STR_TRIM),

        // List
        ListLen => {
            // List structure has already been loaded as i64 (Zig calling convention)
            // We want the second (more significant) 32 bits. Shift and convert to i32.
            code_builder.i64_const(32);
            code_builder.i64_shr_u();
            code_builder.i32_wrap_i64();
        }

        ListGetUnsafe | ListSet | ListSingle | ListRepeat | ListReverse | ListConcat
        | ListContains | ListAppend | ListPrepend | ListJoin | ListRange | ListMap | ListMap2
        | ListMap3 | ListMap4 | ListMapWithIndex | ListKeepIf | ListWalk | ListWalkUntil
        | ListWalkBackwards | ListKeepOks | ListKeepErrs | ListSortWith | ListSublist
        | ListDropAt | ListSwap | ListAny | ListAll | ListFindUnsafe | DictSize | DictEmpty
        | DictInsert | DictRemove | DictContains | DictGetUnsafe | DictKeys | DictValues
        | DictUnion | DictIntersection | DictDifference | DictWalk | SetFromList => {
            return NotImplemented;
        }

        // Num
        NumAdd => match ret_layout {
            WasmLayout::Primitive(value_type, _) => match value_type {
                I32 => code_builder.i32_add(),
                I64 => code_builder.i64_add(),
                F32 => code_builder.f32_add(),
                F64 => code_builder.f64_add(),
            },
            WasmLayout::StackMemory { format, .. } => match format {
                DataStructure => return NotImplemented,
                Int128 => return NotImplemented,
                Float128 => return NotImplemented,
                Decimal => return BuiltinCall(bitcode::DEC_ADD_WITH_OVERFLOW),
            },
        },
        NumAddWrap => match ret_layout {
            WasmLayout::Primitive(value_type, size) => match value_type {
                I32 => {
                    code_builder.i32_add();
                    wrap_i32(code_builder, *size);
                }
                I64 => code_builder.i64_add(),
                F32 => code_builder.f32_add(),
                F64 => code_builder.f64_add(),
            },
            WasmLayout::StackMemory { format, .. } => match format {
                DataStructure => return NotImplemented,
                Int128 => return NotImplemented,
                Float128 => return NotImplemented,
                Decimal => return BuiltinCall(bitcode::DEC_ADD_WITH_OVERFLOW),
            },
        },
        NumToStr => return NotImplemented,
        NumAddChecked => return NotImplemented,
        NumAddSaturated => return NotImplemented,
        NumSub => match ret_layout {
            WasmLayout::Primitive(value_type, _) => match value_type {
                I32 => code_builder.i32_sub(),
                I64 => code_builder.i64_sub(),
                F32 => code_builder.f32_sub(),
                F64 => code_builder.f64_sub(),
            },
            WasmLayout::StackMemory { format, .. } => match format {
                DataStructure => return NotImplemented,
                Int128 => return NotImplemented,
                Float128 => return NotImplemented,
                Decimal => return BuiltinCall(bitcode::DEC_SUB_WITH_OVERFLOW),
            },
        },
        NumSubWrap => match ret_layout {
            WasmLayout::Primitive(value_type, size) => match value_type {
                I32 => {
                    code_builder.i32_sub();
                    wrap_i32(code_builder, *size);
                }
                I64 => code_builder.i64_sub(),
                F32 => code_builder.f32_sub(),
                F64 => code_builder.f64_sub(),
            },
            WasmLayout::StackMemory { format, .. } => match format {
                DataStructure => return NotImplemented,
                Int128 => return NotImplemented,
                Float128 => return NotImplemented,
                Decimal => return BuiltinCall(bitcode::DEC_SUB_WITH_OVERFLOW),
            },
        },
        NumSubChecked => return NotImplemented,
        NumSubSaturated => return NotImplemented,
        NumMul => match ret_layout {
            WasmLayout::Primitive(value_type, _) => match value_type {
                I32 => code_builder.i32_mul(),
                I64 => code_builder.i64_mul(),
                F32 => code_builder.f32_mul(),
                F64 => code_builder.f64_mul(),
            },
            WasmLayout::StackMemory { format, .. } => match format {
                DataStructure => return NotImplemented,
                Int128 => return NotImplemented,
                Float128 => return NotImplemented,
                Decimal => return BuiltinCall(bitcode::DEC_MUL_WITH_OVERFLOW),
            },
        },
        NumMulWrap => match ret_layout {
            WasmLayout::Primitive(value_type, size) => match value_type {
                I32 => {
                    code_builder.i32_mul();
                    wrap_i32(code_builder, *size);
                }
                I64 => code_builder.i64_mul(),
                F32 => code_builder.f32_mul(),
                F64 => code_builder.f64_mul(),
            },
            WasmLayout::StackMemory { format, .. } => match format {
                DataStructure => return NotImplemented,
                Int128 => return NotImplemented,
                Float128 => return NotImplemented,
                Decimal => return BuiltinCall(bitcode::DEC_MUL_WITH_OVERFLOW),
            },
        },
        NumMulChecked => return NotImplemented,
        NumGt => match storage.get(&args[0]) {
            StoredValue::VirtualMachineStack { value_type, .. }
            | StoredValue::Local { value_type, .. } => match value_type {
                I32 => code_builder.i32_gt_s(),
                I64 => code_builder.i64_gt_s(),
                F32 => code_builder.f32_gt(),
                F64 => code_builder.f64_gt(),
            },
            StoredValue::StackMemory { .. } => return NotImplemented,
        },
        NumGte => match storage.get(&args[0]) {
            StoredValue::VirtualMachineStack { value_type, .. }
            | StoredValue::Local { value_type, .. } => match value_type {
                I32 => code_builder.i32_ge_s(),
                I64 => code_builder.i64_ge_s(),
                F32 => code_builder.f32_ge(),
                F64 => code_builder.f64_ge(),
            },
            StoredValue::StackMemory { .. } => return NotImplemented,
        },
        NumLt => match storage.get(&args[0]) {
            StoredValue::VirtualMachineStack { value_type, .. }
            | StoredValue::Local { value_type, .. } => match value_type {
                I32 => code_builder.i32_lt_s(),
                I64 => code_builder.i64_lt_s(),
                F32 => code_builder.f32_lt(),
                F64 => code_builder.f64_lt(),
            },
            StoredValue::StackMemory { .. } => return NotImplemented,
        },
        NumLte => match storage.get(&args[0]) {
            StoredValue::VirtualMachineStack { value_type, .. }
            | StoredValue::Local { value_type, .. } => match value_type {
                I32 => code_builder.i32_le_s(),
                I64 => code_builder.i64_le_s(),
                F32 => code_builder.f32_le(),
                F64 => code_builder.f64_le(),
            },
            StoredValue::StackMemory { .. } => return NotImplemented,
        },
        NumCompare => return NotImplemented,
        NumDivUnchecked => match storage.get(&args[0]) {
            StoredValue::VirtualMachineStack { value_type, .. }
            | StoredValue::Local { value_type, .. } => match value_type {
                I32 => code_builder.i32_div_s(),
                I64 => code_builder.i64_div_s(),
                F32 => code_builder.f32_div(),
                F64 => code_builder.f64_div(),
            },
            StoredValue::StackMemory { format, .. } => match format {
                DataStructure => return NotImplemented,
                Int128 => return NotImplemented,
                Float128 => return NotImplemented,
                Decimal => return BuiltinCall(bitcode::DEC_DIV),
            },
        },
        NumDivCeilUnchecked => return NotImplemented,
        NumRemUnchecked => match storage.get(&args[0]) {
            StoredValue::VirtualMachineStack { value_type, .. }
            | StoredValue::Local { value_type, .. } => match value_type {
                I32 => code_builder.i32_rem_s(),
                I64 => code_builder.i64_rem_s(),
                F32 => return NotImplemented,
                F64 => return NotImplemented,
            },
            StoredValue::StackMemory { .. } => return NotImplemented,
        },
        NumIsMultipleOf => return NotImplemented,
        NumAbs => match storage.get(&args[0]) {
            StoredValue::VirtualMachineStack { value_type, .. }
            | StoredValue::Local { value_type, .. } => match value_type {
                I32 => {
                    let arg_storage = storage.get(&args[0]).to_owned();
                    storage.ensure_value_has_local(code_builder, args[0], arg_storage);
                    storage.load_symbols(code_builder, args);
                    code_builder.i32_const(0);
                    storage.load_symbols(code_builder, args);
                    code_builder.i32_sub();
                    storage.load_symbols(code_builder, args);
                    code_builder.i32_const(0);
                    code_builder.i32_ge_s();
                    code_builder.select();
                }
                I64 => {
                    let arg_storage = storage.get(&args[0]).to_owned();
                    storage.ensure_value_has_local(code_builder, args[0], arg_storage);
                    storage.load_symbols(code_builder, args);
                    code_builder.i64_const(0);
                    storage.load_symbols(code_builder, args);
                    code_builder.i64_sub();
                    storage.load_symbols(code_builder, args);
                    code_builder.i64_const(0);
                    code_builder.i64_ge_s();
                    code_builder.select();
                }
                F32 => code_builder.f32_abs(),
                F64 => code_builder.f64_abs(),
            },
            StoredValue::StackMemory { .. } => return NotImplemented,
        },
        NumNeg => match ret_layout {
            WasmLayout::Primitive(value_type, _) => match value_type {
                I32 => {
                    code_builder.i32_const(0);
                    storage.load_symbols(code_builder, args);
                    code_builder.i32_sub();
                }
                I64 => {
                    code_builder.i64_const(0);
                    storage.load_symbols(code_builder, args);
                    code_builder.i64_sub();
                }
                F32 => code_builder.f32_neg(),
                F64 => code_builder.f64_neg(),
            },
            WasmLayout::StackMemory { .. } => return NotImplemented,
        },
        NumSin => return NotImplemented,
        NumCos => return NotImplemented,
        NumSqrtUnchecked => return NotImplemented,
        NumLogUnchecked => return NotImplemented,
        NumRound => match storage.get(&args[0]) {
            StoredValue::VirtualMachineStack { value_type, .. }
            | StoredValue::Local { value_type, .. } => match value_type {
                F32 => return BuiltinCall(&bitcode::NUM_ROUND[FloatWidth::F32]),
                F64 => return BuiltinCall(&bitcode::NUM_ROUND[FloatWidth::F64]),
                _ => return NotImplemented,
            },
            StoredValue::StackMemory { .. } => return NotImplemented,
        },
        NumToFloat => {
            use StoredValue::*;
            let stored = storage.get(&args[0]);
            match ret_layout {
                WasmLayout::Primitive(ret_type, _) => match stored {
                    VirtualMachineStack { value_type, .. } | Local { value_type, .. } => {
                        match (ret_type, value_type) {
                            (F32, I32) => code_builder.f32_convert_s_i32(),
                            (F32, I64) => code_builder.f32_convert_s_i64(),
                            (F32, F32) => {}
                            (F32, F64) => code_builder.f32_demote_f64(),

                            (F64, I32) => code_builder.f64_convert_s_i32(),
                            (F64, I64) => code_builder.f64_convert_s_i64(),
                            (F64, F32) => code_builder.f64_promote_f32(),
                            (F64, F64) => {}

                            _ => panic_ret_type(),
                        }
                    }
                    StackMemory { .. } => return NotImplemented,
                },
                WasmLayout::StackMemory { .. } => return NotImplemented,
            }
        }
        NumPow => return NotImplemented,
        NumCeiling => match ret_layout {
            WasmLayout::Primitive(value_type, _) => match value_type {
                I32 => {
                    code_builder.f32_ceil();
                    code_builder.i32_trunc_s_f32()
                }
                I64 => {
                    code_builder.f64_ceil();
                    code_builder.i64_trunc_s_f64()
                }
                _ => panic_ret_type(),
            },
            WasmLayout::StackMemory { .. } => return NotImplemented,
        },
        NumPowInt => return NotImplemented,
        NumFloor => match ret_layout {
            WasmLayout::Primitive(value_type, _) => match value_type {
                I32 => {
                    code_builder.f32_floor();
                    code_builder.i32_trunc_s_f32()
                }
                I64 => {
                    code_builder.f64_floor();
                    code_builder.i64_trunc_s_f64()
                }
                _ => panic_ret_type(),
            },
            WasmLayout::StackMemory { .. } => return NotImplemented,
        },
        NumIsFinite => {
            use StoredValue::*;
            match storage.get(&args[0]) {
                VirtualMachineStack { value_type, .. } | Local { value_type, .. } => {
                    match value_type {
                        I32 | I64 => code_builder.i32_const(1), // always true for integers
                        F32 => {
                            code_builder.i32_reinterpret_f32();
                            code_builder.i32_const(0x7f80_0000);
                            code_builder.i32_and();
                            code_builder.i32_const(0x7f80_0000);
                            code_builder.i32_ne();
                        }
                        F64 => {
                            code_builder.i64_reinterpret_f64();
                            code_builder.i64_const(0x7ff0_0000_0000_0000);
                            code_builder.i64_and();
                            code_builder.i64_const(0x7ff0_0000_0000_0000);
                            code_builder.i64_ne();
                        }
                    }
                }
                StackMemory {
                    format, location, ..
                } => {
                    let (local_id, offset) = location.local_and_offset(storage.stack_frame_pointer);

                    match format {
                        Int128 => code_builder.i32_const(1),
                        Float128 => {
                            code_builder.get_local(local_id);
                            code_builder.i64_load(Align::Bytes4, offset + 8);
                            code_builder.i64_const(0x7fff_0000_0000_0000);
                            code_builder.i64_and();
                            code_builder.i64_const(0x7fff_0000_0000_0000);
                            code_builder.i64_ne();
                        }
                        Decimal => {
                            code_builder.get_local(local_id);
                            code_builder.i64_load(Align::Bytes4, offset + 8);
                            code_builder.i64_const(0x7100_0000_0000_0000);
                            code_builder.i64_and();
                            code_builder.i64_const(0x7100_0000_0000_0000);
                            code_builder.i64_ne();
                        }
                        DataStructure => return NotImplemented,
                    }
                }
            }
        }
        NumAtan => {
            let width = float_width_from_layout(ret_layout);
            return BuiltinCall(&bitcode::NUM_ATAN[width]);
        }
        NumAcos => {
            let width = float_width_from_layout(ret_layout);
            return BuiltinCall(&bitcode::NUM_ACOS[width]);
        }
        NumAsin => {
            let width = float_width_from_layout(ret_layout);
            return BuiltinCall(&bitcode::NUM_ASIN[width]);
        }
        NumBytesToU16 => return NotImplemented,
        NumBytesToU32 => return NotImplemented,
        NumBitwiseAnd => match ret_layout {
            WasmLayout::Primitive(value_type, _) => match value_type {
                I32 => code_builder.i32_and(),
                I64 => code_builder.i64_and(),
                _ => panic_ret_type(),
            },
            WasmLayout::StackMemory { .. } => return NotImplemented,
        },
        NumBitwiseXor => match ret_layout {
            WasmLayout::Primitive(value_type, _) => match value_type {
                I32 => code_builder.i32_xor(),
                I64 => code_builder.i64_xor(),
                _ => panic_ret_type(),
            },
            WasmLayout::StackMemory { .. } => return NotImplemented,
        },
        NumBitwiseOr => match ret_layout {
            WasmLayout::Primitive(value_type, _) => match value_type {
                I32 => code_builder.i32_or(),
                I64 => code_builder.i64_or(),
                _ => panic_ret_type(),
            },
            WasmLayout::StackMemory { .. } => return NotImplemented,
        },
        NumShiftLeftBy => {
            // Swap order of arguments
            storage.load_symbols(code_builder, &[args[1], args[0]]);
            match ret_layout {
                WasmLayout::Primitive(value_type, _) => match value_type {
                    I32 => code_builder.i32_shl(),
                    I64 => code_builder.i64_shl(),
                    _ => panic_ret_type(),
                },
                WasmLayout::StackMemory { .. } => return NotImplemented,
            }
        }
        NumShiftRightBy => match ret_layout {
            WasmLayout::Primitive(value_type, _) => match value_type {
                I32 => code_builder.i32_shr_s(),
                I64 => code_builder.i64_shr_s(),
                _ => panic_ret_type(),
            },
            WasmLayout::StackMemory { .. } => return NotImplemented,
        },
        NumShiftRightZfBy => match ret_layout {
            WasmLayout::Primitive(value_type, _) => match value_type {
                I32 => code_builder.i32_shr_u(),
                I64 => code_builder.i64_shr_u(),
                _ => panic_ret_type(),
            },
            WasmLayout::StackMemory { .. } => return NotImplemented,
        },
        NumIntCast => {
            use StoredValue::*;
            let stored = storage.get(&args[0]);
            match ret_layout {
                WasmLayout::Primitive(ret_type, _) => match stored {
                    VirtualMachineStack { value_type, .. } | Local { value_type, .. } => {
                        match (ret_type, value_type) {
                            (I32, I32) => {}
                            (I32, I64) => code_builder.i32_wrap_i64(),
                            (I32, F32) => code_builder.i32_trunc_s_f32(),
                            (I32, F64) => code_builder.i32_trunc_s_f64(),

                            (I64, I32) => code_builder.i64_extend_s_i32(),
                            (I64, I64) => {}
                            (I64, F32) => code_builder.i64_trunc_s_f32(),
                            (I64, F64) => code_builder.i64_trunc_s_f64(),

                            (F32, I32) => code_builder.f32_convert_s_i32(),
                            (F32, I64) => code_builder.f32_convert_s_i64(),
                            (F32, F32) => {}
                            (F32, F64) => code_builder.f32_demote_f64(),

                            (F64, I32) => code_builder.f64_convert_s_i32(),
                            (F64, I64) => code_builder.f64_convert_s_i64(),
                            (F64, F32) => code_builder.f64_promote_f32(),
                            (F64, F64) => {}
                        }
                    }

                    StackMemory { .. } => return NotImplemented,
                },
                WasmLayout::StackMemory { .. } => return NotImplemented,
            }
        }
        And => code_builder.i32_and(),
        Or => code_builder.i32_or(),
        Not => code_builder.i32_eqz(),
        ExpectTrue => return NotImplemented,
        RefCountInc => return BuiltinCall(bitcode::UTILS_INCREF),
        RefCountDec => return BuiltinCall(bitcode::UTILS_DECREF),
        Eq | NotEq | Hash | PtrCast => {
            internal_error!("{:?} should be handled in backend.rs", lowlevel)
        }
    }
    Done
}

/// Wrap an integer whose Wasm representation is i32
fn wrap_i32(code_builder: &mut CodeBuilder, size: u32) {
    match size {
        1 => {
            // Underlying Roc value is i8
            code_builder.i32_const(24);
            code_builder.i32_shl();
            code_builder.i32_const(24);
            code_builder.i32_shr_s();
        }
        2 => {
            // Underlying Roc value is i16
            code_builder.i32_const(16);
            code_builder.i32_shl();
            code_builder.i32_const(16);
            code_builder.i32_shr_s();
        }
        _ => {} // the only other possible value is 4, and i32 wraps natively
    }
}

fn float_width_from_layout(wasm_layout: &WasmLayout) -> FloatWidth {
    match wasm_layout {
        WasmLayout::Primitive(F32, _) => FloatWidth::F32,
        WasmLayout::Primitive(F64, _) => FloatWidth::F64,
        _ => internal_error!("{:?} does not have a FloatWidth", wasm_layout),
    }
}
