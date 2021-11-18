use roc_builtins::bitcode::{self, FloatWidth};
use roc_module::low_level::{LowLevel, LowLevel::*};
use roc_module::symbol::Symbol;

use crate::layout::WasmLayout;
use crate::storage::Storage;
use crate::wasm_module::{
    CodeBuilder,
    ValueType::{self, *},
};

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

    let panic_ret_type = || panic!("Invalid return layout for {:?}: {:?}", lowlevel, ret_layout);

    match lowlevel {
        StrConcat => return BuiltinCall(bitcode::STR_CONCAT),

        StrJoinWith | StrIsEmpty | StrStartsWith | StrStartsWithCodePt | StrEndsWith | StrSplit
        | StrCountGraphemes | StrFromInt | StrFromUtf8 | StrTrimLeft | StrTrimRight
        | StrFromUtf8Range | StrToUtf8 | StrRepeat | StrFromFloat | StrTrim | ListLen
        | ListGetUnsafe | ListSet | ListSingle | ListRepeat | ListReverse | ListConcat
        | ListContains | ListAppend | ListPrepend | ListJoin | ListRange | ListMap | ListMap2
        | ListMap3 | ListMap4 | ListMapWithIndex | ListKeepIf | ListWalk | ListWalkUntil
        | ListWalkBackwards | ListKeepOks | ListKeepErrs | ListSortWith | ListSublist
        | ListDropAt | ListSwap | ListAny | ListFindUnsafe | DictSize | DictEmpty | DictInsert
        | DictRemove | DictContains | DictGetUnsafe | DictKeys | DictValues | DictUnion
        | DictIntersection | DictDifference | DictWalk | SetFromList => {
            return NotImplemented;
        }

        NumAdd => match ret_layout.value_type() {
            I32 => code_builder.i32_add(),
            I64 => code_builder.i64_add(),
            F32 => code_builder.f32_add(),
            F64 => code_builder.f64_add(),
        },
        NumAddWrap => match ret_layout.value_type() {
            I32 => {
                code_builder.i32_add();
                wrap_i32(code_builder, ret_layout.size());
            }
            I64 => code_builder.i64_add(),
            F32 => code_builder.f32_add(),
            F64 => code_builder.f64_add(),
        },
        NumAddChecked => return NotImplemented,
        NumSub => match ret_layout.value_type() {
            I32 => code_builder.i32_sub(),
            I64 => code_builder.i64_sub(),
            F32 => code_builder.f32_sub(),
            F64 => code_builder.f64_sub(),
        },
        NumSubWrap => match ret_layout.value_type() {
            I32 => {
                code_builder.i32_sub();
                wrap_i32(code_builder, ret_layout.size());
            }
            I64 => code_builder.i64_sub(),
            F32 => code_builder.f32_sub(),
            F64 => code_builder.f64_sub(),
        },
        NumSubChecked => return NotImplemented,
        NumMul => match ret_layout.value_type() {
            I32 => code_builder.i32_mul(),
            I64 => code_builder.i64_mul(),
            F32 => code_builder.f32_mul(),
            F64 => code_builder.f64_mul(),
        },
        NumMulWrap => match ret_layout.value_type() {
            I32 => {
                code_builder.i32_mul();
                wrap_i32(code_builder, ret_layout.size());
            }
            I64 => code_builder.i64_mul(),
            F32 => code_builder.f32_mul(),
            F64 => code_builder.f64_mul(),
        },
        NumMulChecked => return NotImplemented,
        NumGt => match storage.get(&args[0]).value_type() {
            I32 => code_builder.i32_gt_s(),
            I64 => code_builder.i64_gt_s(),
            F32 => code_builder.f32_gt(),
            F64 => code_builder.f64_gt(),
        },
        NumGte => match storage.get(&args[0]).value_type() {
            I32 => code_builder.i32_ge_s(),
            I64 => code_builder.i64_ge_s(),
            F32 => code_builder.f32_ge(),
            F64 => code_builder.f64_ge(),
        },
        NumLt => match storage.get(&args[0]).value_type() {
            I32 => code_builder.i32_lt_s(),
            I64 => code_builder.i64_lt_s(),
            F32 => code_builder.f32_lt(),
            F64 => code_builder.f64_lt(),
        },
        NumLte => match storage.get(&args[0]).value_type() {
            I32 => code_builder.i32_le_s(),
            I64 => code_builder.i64_le_s(),
            F32 => code_builder.f32_le(),
            F64 => code_builder.f64_le(),
        },
        NumCompare => return NotImplemented,
        NumDivUnchecked => match ret_layout.value_type() {
            I32 => code_builder.i32_div_s(),
            I64 => code_builder.i64_div_s(),
            F32 => code_builder.f32_div(),
            F64 => code_builder.f64_div(),
        },
        NumDivCeilUnchecked => return NotImplemented,
        NumRemUnchecked => match ret_layout.value_type() {
            I32 => code_builder.i32_rem_s(),
            I64 => code_builder.i64_rem_s(),
            F32 => return NotImplemented,
            F64 => return NotImplemented,
        },
        NumIsMultipleOf => return NotImplemented,
        NumAbs => match ret_layout.value_type() {
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
        NumNeg => {
            match ret_layout.value_type() {
                I32 => {
                    // Unfortunate local.set/local.get
                    code_builder.i32_const(0);
                    storage.load_symbols(code_builder, args);
                    code_builder.i32_sub();
                }
                I64 => {
                    // Unfortunate local.set/local.get
                    code_builder.i64_const(0);
                    storage.load_symbols(code_builder, args);
                    code_builder.i64_sub();
                }
                F32 => code_builder.f32_neg(),
                F64 => code_builder.f64_neg(),
            }
        }
        NumSin => return NotImplemented,
        NumCos => return NotImplemented,
        NumSqrtUnchecked => return NotImplemented,
        NumLogUnchecked => return NotImplemented,
        NumRound => {
            // FIXME
            // thread 'gen_num::f64_round' panicked at 'called `Result::unwrap()` on an `Err` value:
            // Io(Os { code: 2, kind: NotFound, message: "No such file or directory" })',
            // compiler/test_gen/src/helpers/wasm.rs:185:53
            // Note: Wasm has a `nearest` op, but it does round-to-even when fraction is exactly 0.5
            // which fails tests. Will this do? Or is specific behaviour important?
            let width = float_width_from_layout(ret_layout);
            return BuiltinCall(&bitcode::NUM_ROUND[width]);
        }
        NumToFloat => match (ret_layout.value_type(), storage.get(&args[0]).value_type()) {
            (F32, I32) => code_builder.f32_convert_s_i32(),
            (F32, I64) => code_builder.f32_convert_s_i64(),
            (F32, F32) => {}
            (F32, F64) => code_builder.f32_demote_f64(),
            (F64, I32) => code_builder.f64_convert_s_i32(),
            (F64, I64) => code_builder.f64_convert_s_i64(),
            (F64, F32) => code_builder.f64_promote_f32(),
            (F64, F64) => {}
            _ => panic_ret_type(),
        },
        NumPow => return NotImplemented,
        NumCeiling => match ret_layout.value_type() {
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
        NumPowInt => return NotImplemented,
        NumFloor => match ret_layout.value_type() {
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
        NumIsFinite => match ret_layout.value_type() {
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
        NumBitwiseAnd => match ret_layout.value_type() {
            I32 => code_builder.i32_and(),
            I64 => code_builder.i64_and(),
            _ => panic_ret_type(),
        },
        NumBitwiseXor => match ret_layout.value_type() {
            I32 => code_builder.i32_xor(),
            I64 => code_builder.i64_xor(),
            _ => panic_ret_type(),
        },
        NumBitwiseOr => match ret_layout.value_type() {
            I32 => code_builder.i32_or(),
            I64 => code_builder.i64_or(),
            _ => panic_ret_type(),
        },
        NumShiftLeftBy => {
            // Unfortunate local.set/local.get
            storage.load_symbols(code_builder, &[args[1], args[0]]);
            match ret_layout.value_type() {
                I32 => code_builder.i32_shl(),
                I64 => code_builder.i64_shl(),
                _ => panic_ret_type(),
            }
        }
        NumShiftRightBy => match ret_layout.value_type() {
            I32 => code_builder.i32_shr_s(),
            I64 => code_builder.i64_shr_s(),
            _ => panic_ret_type(),
        },
        NumShiftRightZfBy => match ret_layout.value_type() {
            I32 => code_builder.i32_shr_u(),
            I64 => code_builder.i64_shr_u(),
            _ => panic_ret_type(),
        },
        NumIntCast => match (ret_layout.value_type(), storage.get(&args[0]).value_type()) {
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
        },
        Eq => {
            // TODO: For non-number types, this will implement pointer equality, which is wrong
            match storage.get(&args[0]).value_type() {
                I32 => code_builder.i32_eq(),
                I64 => code_builder.i64_eq(),
                F32 => code_builder.f32_eq(),
                F64 => code_builder.f64_eq(),
            }
        }
        NotEq => {
            // TODO: For non-number types, this will implement pointer inequality, which is wrong
            match storage.get(&args[0]).value_type() {
                I32 => code_builder.i32_ne(),
                I64 => code_builder.i64_ne(),
                F32 => code_builder.f32_ne(),
                F64 => code_builder.f64_ne(),
            }
        }
        And => code_builder.i32_and(),
        Or => code_builder.i32_or(),
        Not => code_builder.i32_eqz(),
        Hash => return NotImplemented,
        ExpectTrue => return NotImplemented,
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
    if wasm_layout.value_type() == ValueType::F32 {
        FloatWidth::F32
    } else {
        FloatWidth::F64
    }
}
