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
        StrConcat | StrJoinWith | StrIsEmpty | StrStartsWith | StrStartsWithCodePt
        | StrEndsWith | StrSplit | StrCountGraphemes | StrFromInt | StrFromUtf8 | StrTrimLeft
        | StrTrimRight | StrFromUtf8Range | StrToUtf8 | StrRepeat | StrFromFloat | StrTrim
        | ListLen | ListGetUnsafe | ListSet | ListSingle | ListRepeat | ListReverse
        | ListConcat | ListContains | ListAppend | ListPrepend | ListJoin | ListRange | ListMap
        | ListMap2 | ListMap3 | ListMap4 | ListMapWithIndex | ListKeepIf | ListWalk
        | ListWalkUntil | ListWalkBackwards | ListKeepOks | ListKeepErrs | ListSortWith
        | ListSublist | ListDrop | ListDropAt | ListSwap | ListAny | ListFindUnsafe | DictSize
        | DictEmpty | DictInsert | DictRemove | DictContains | DictGetUnsafe | DictKeys
        | DictValues | DictUnion | DictIntersection | DictDifference | DictWalk | SetFromList => {
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

pub fn symbol_to_lowlevel(symbol: Symbol) -> Option<LowLevel> {
    use LowLevel::*;

    match symbol {
        Symbol::STR_CONCAT => Some(StrConcat),
        Symbol::STR_JOIN_WITH => Some(StrJoinWith),
        Symbol::STR_IS_EMPTY => Some(StrIsEmpty),
        Symbol::STR_STARTS_WITH => Some(StrStartsWith),
        Symbol::STR_STARTS_WITH_CODE_PT => Some(StrStartsWithCodePt),
        Symbol::STR_ENDS_WITH => Some(StrEndsWith),
        Symbol::STR_SPLIT => Some(StrSplit),
        Symbol::STR_COUNT_GRAPHEMES => Some(StrCountGraphemes),
        Symbol::STR_FROM_INT => Some(StrFromInt),
        Symbol::STR_FROM_UTF8 => Some(StrFromUtf8),
        Symbol::STR_FROM_UTF8_RANGE => Some(StrFromUtf8Range),
        Symbol::STR_TO_UTF8 => Some(StrToUtf8),
        Symbol::STR_REPEAT => Some(StrRepeat),
        Symbol::STR_FROM_FLOAT => Some(StrFromFloat),
        Symbol::STR_TRIM => Some(StrTrim),
        Symbol::STR_TRIM_LEFT => Some(StrTrimLeft),
        Symbol::STR_TRIM_RIGHT => Some(StrTrimRight),
        Symbol::LIST_LEN => Some(ListLen),
        Symbol::LIST_GET => Some(ListGetUnsafe), // ??
        Symbol::LIST_SET => Some(ListSet),
        Symbol::LIST_SINGLE => Some(ListSingle),
        Symbol::LIST_REPEAT => Some(ListRepeat),
        Symbol::LIST_REVERSE => Some(ListReverse),
        Symbol::LIST_CONCAT => Some(ListConcat),
        Symbol::LIST_CONTAINS => Some(ListContains),
        Symbol::LIST_APPEND => Some(ListAppend),
        Symbol::LIST_PREPEND => Some(ListPrepend),
        Symbol::LIST_JOIN => Some(ListJoin),
        Symbol::LIST_RANGE => Some(ListRange),
        Symbol::LIST_MAP => Some(ListMap),
        Symbol::LIST_MAP2 => Some(ListMap2),
        Symbol::LIST_MAP3 => Some(ListMap3),
        Symbol::LIST_MAP4 => Some(ListMap4),
        Symbol::LIST_MAP_WITH_INDEX => Some(ListMapWithIndex),
        Symbol::LIST_KEEP_IF => Some(ListKeepIf),
        Symbol::LIST_WALK => Some(ListWalk),
        Symbol::LIST_WALK_UNTIL => Some(ListWalkUntil),
        Symbol::LIST_WALK_BACKWARDS => Some(ListWalkBackwards),
        Symbol::LIST_KEEP_OKS => Some(ListKeepOks),
        Symbol::LIST_KEEP_ERRS => Some(ListKeepErrs),
        Symbol::LIST_SORT_WITH => Some(ListSortWith),
        Symbol::LIST_SUBLIST => Some(ListSublist),

        Symbol::LIST_DROP => Some(ListDrop),
        Symbol::LIST_DROP_AT => Some(ListDropAt),
        Symbol::LIST_SWAP => Some(ListSwap),
        Symbol::LIST_ANY => Some(ListAny),
        Symbol::LIST_FIND => Some(ListFindUnsafe), // ??
        Symbol::DICT_LEN => Some(DictSize),
        Symbol::DICT_EMPTY => Some(DictEmpty),
        Symbol::DICT_INSERT => Some(DictInsert),
        Symbol::DICT_REMOVE => Some(DictRemove),
        Symbol::DICT_CONTAINS => Some(DictContains),
        Symbol::DICT_GET => Some(DictGetUnsafe), // ??
        Symbol::DICT_KEYS => Some(DictKeys),
        Symbol::DICT_VALUES => Some(DictValues),
        Symbol::DICT_UNION => Some(DictUnion),
        Symbol::DICT_INTERSECTION => Some(DictIntersection),
        Symbol::DICT_DIFFERENCE => Some(DictDifference),
        Symbol::DICT_WALK => Some(DictWalk),
        Symbol::SET_FROM_LIST => Some(SetFromList),

        Symbol::NUM_ADD => Some(NumAdd),
        Symbol::NUM_ADD_WRAP => Some(NumAddWrap),
        Symbol::NUM_ADD_CHECKED => Some(NumAddChecked),
        Symbol::NUM_SUB => Some(NumSub),
        Symbol::NUM_SUB_WRAP => Some(NumSubWrap),
        Symbol::NUM_SUB_CHECKED => Some(NumSubChecked),
        Symbol::NUM_MUL => Some(NumMul),
        Symbol::NUM_MUL_WRAP => Some(NumMulWrap),
        Symbol::NUM_MUL_CHECKED => Some(NumMulChecked),
        Symbol::NUM_GT => Some(NumGt),
        Symbol::NUM_GTE => Some(NumGte),
        Symbol::NUM_LT => Some(NumLt),
        Symbol::NUM_LTE => Some(NumLte),
        Symbol::NUM_COMPARE => Some(NumCompare),
        Symbol::NUM_DIV_FLOAT => Some(NumDivUnchecked), // ??
        Symbol::NUM_DIV_CEIL => Some(NumDivCeilUnchecked),
        Symbol::NUM_REM => Some(NumRemUnchecked),
        Symbol::NUM_IS_MULTIPLE_OF => Some(NumIsMultipleOf),
        Symbol::NUM_ABS => Some(NumAbs),
        Symbol::NUM_NEG => Some(NumNeg),
        Symbol::NUM_SIN => Some(NumSin),
        Symbol::NUM_COS => Some(NumCos),
        Symbol::NUM_SQRT => Some(NumSqrtUnchecked),
        Symbol::NUM_LOG => Some(NumLogUnchecked),
        Symbol::NUM_ROUND => Some(NumRound),
        Symbol::NUM_TO_FLOAT => Some(NumToFloat),
        Symbol::NUM_POW => Some(NumPow),
        Symbol::NUM_CEILING => Some(NumCeiling),
        Symbol::NUM_POW_INT => Some(NumPowInt),
        Symbol::NUM_FLOOR => Some(NumFloor),
        // => Some(NumIsFinite),
        Symbol::NUM_ATAN => Some(NumAtan),
        Symbol::NUM_ACOS => Some(NumAcos),
        Symbol::NUM_ASIN => Some(NumAsin),
        Symbol::NUM_BYTES_TO_U16 => Some(NumBytesToU16),
        Symbol::NUM_BYTES_TO_U32 => Some(NumBytesToU32),
        Symbol::NUM_BITWISE_AND => Some(NumBitwiseAnd),
        Symbol::NUM_BITWISE_XOR => Some(NumBitwiseXor),
        Symbol::NUM_BITWISE_OR => Some(NumBitwiseOr),
        Symbol::NUM_SHIFT_LEFT => Some(NumShiftLeftBy),
        Symbol::NUM_SHIFT_RIGHT => Some(NumShiftRightBy),
        Symbol::NUM_SHIFT_RIGHT_ZERO_FILL => Some(NumShiftRightZfBy),
        Symbol::NUM_INT_CAST => Some(NumIntCast),
        Symbol::BOOL_EQ => Some(Eq),
        Symbol::BOOL_NEQ => Some(NotEq),
        Symbol::BOOL_AND => Some(And),
        Symbol::BOOL_OR => Some(Or),
        Symbol::BOOL_NOT => Some(Not),
        // => Some(Hash),
        // => Some(ExpectTrue),
        _ => None,
    }
}
