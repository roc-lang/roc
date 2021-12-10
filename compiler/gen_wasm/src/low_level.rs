use roc_builtins::bitcode::{self, FloatWidth};
use roc_module::low_level::{LowLevel, LowLevel::*};
use roc_module::symbol::Symbol;
use roc_reporting::internal_error;

use crate::layout::{StackMemoryFormat::*, WasmLayout};
use crate::storage::{Storage, StoredValue};
use crate::wasm_module::{CodeBuilder, ValueType::*};

pub enum LowlevelBuildResult {
    Done,
    BuiltinCall(&'static str),
    NotImplemented,
}

pub fn decode_low_level<'a>(
    code_builder: &mut CodeBuilder<'a>,
    storage: &mut Storage<'a>,
    lowlevel: LowLevel,
    args: &'a [Symbol],
    ret_layout: &WasmLayout,
) -> LowlevelBuildResult {
    use LowlevelBuildResult::*;

    let panic_ret_type =
        || internal_error!("Invalid return layout for {:?}: {:?}", lowlevel, ret_layout);

    match lowlevel {
        StrConcat => return BuiltinCall(bitcode::STR_CONCAT),
        StrJoinWith => return NotImplemented, // needs Array
        StrIsEmpty => {
            code_builder.i64_const(i64::MIN);
            code_builder.i64_eq();
        }
        StrStartsWith => return BuiltinCall(bitcode::STR_STARTS_WITH),
        StrStartsWithCodePt => return BuiltinCall(bitcode::STR_STARTS_WITH_CODE_PT),
        StrEndsWith => return BuiltinCall(bitcode::STR_ENDS_WITH),
        StrSplit => return NotImplemented,          // needs Array
        StrCountGraphemes => return NotImplemented, // test needs Array
        StrToNum => return NotImplemented,          // choose builtin based on storage size
        StrFromInt => return NotImplemented,        // choose builtin based on storage size
        StrFromUtf8 => return NotImplemented,       // needs Array
        StrTrimLeft => return BuiltinCall(bitcode::STR_TRIM_LEFT),
        StrTrimRight => return BuiltinCall(bitcode::STR_TRIM_RIGHT),
        StrFromUtf8Range => return NotImplemented, // needs Array
        StrToUtf8 => return NotImplemented,        // needs Array
        StrRepeat => return BuiltinCall(bitcode::STR_REPEAT),
        StrFromFloat => {
            // linker errors for __ashlti3, __fixunsdfti, __multi3, __udivti3, __umodti3
            // https://gcc.gnu.org/onlinedocs/gccint/Integer-library-routines.html
            // https://gcc.gnu.org/onlinedocs/gccint/Soft-float-library-routines.html
            return NotImplemented;
        }
        StrTrim => return BuiltinCall(bitcode::STR_TRIM),

        ListLen | ListGetUnsafe | ListSet | ListSingle | ListRepeat | ListReverse | ListConcat
        | ListContains | ListAppend | ListPrepend | ListJoin | ListRange | ListMap | ListMap2
        | ListMap3 | ListMap4 | ListMapWithIndex | ListKeepIf | ListWalk | ListWalkUntil
        | ListWalkBackwards | ListKeepOks | ListKeepErrs | ListSortWith | ListSublist
        | ListDropAt | ListSwap | ListAny | ListAll | ListFindUnsafe | DictSize | DictEmpty
        | DictInsert | DictRemove | DictContains | DictGetUnsafe | DictKeys | DictValues
        | DictUnion | DictIntersection | DictDifference | DictWalk | SetFromList => {
            return NotImplemented;
        }

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
                    // TODO: is *deliberate* wrapping really in the spirit of things?
                    // The point of choosing NumAddWrap is to go fast by skipping checks, but we're making it slower.
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
        NumIsFinite => match ret_layout {
            WasmLayout::Primitive(value_type, _) => match value_type {
                I32 => code_builder.i32_const(1),
                I64 => code_builder.i32_const(1),
                F32 => {
                    code_builder.i32_reinterpret_f32();
                    code_builder.i32_const(0x7f800000);
                    code_builder.i32_and();
                    code_builder.i32_const(0x7f800000);
                    code_builder.i32_ne();
                }
                F64 => {
                    code_builder.i64_reinterpret_f64();
                    code_builder.i64_const(0x7ff0000000000000);
                    code_builder.i64_and();
                    code_builder.i64_const(0x7ff0000000000000);
                    code_builder.i64_ne();
                }
            },
            WasmLayout::StackMemory { .. } => return NotImplemented,
        },
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
        Eq => match storage.get(&args[0]) {
            StoredValue::VirtualMachineStack { value_type, .. }
            | StoredValue::Local { value_type, .. } => match value_type {
                I32 => code_builder.i32_eq(),
                I64 => code_builder.i64_eq(),
                F32 => code_builder.f32_eq(),
                F64 => code_builder.f64_eq(),
            },
            StoredValue::StackMemory { .. } => return NotImplemented,
        },
        NotEq => match storage.get(&args[0]) {
            StoredValue::VirtualMachineStack { value_type, .. }
            | StoredValue::Local { value_type, .. } => match value_type {
                I32 => code_builder.i32_ne(),
                I64 => code_builder.i64_ne(),
                F32 => code_builder.f32_ne(),
                F64 => code_builder.f64_ne(),
            },
            StoredValue::StackMemory { .. } => return NotImplemented,
        },
        And => code_builder.i32_and(),
        Or => code_builder.i32_or(),
        Not => code_builder.i32_eqz(),
        Hash => return NotImplemented,
        ExpectTrue => return NotImplemented,
        RefCountGetPtr => {
            code_builder.i32_const(4);
            code_builder.i32_sub();
        }
        RefCountInc => return BuiltinCall(bitcode::UTILS_INCREF),
        RefCountDec => return BuiltinCall(bitcode::UTILS_DECREF),
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
