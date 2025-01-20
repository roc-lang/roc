use crate::def::Def;
use crate::expr::{AnnotatedMark, ClosureData, Expr::*};
use crate::expr::{Expr, Recursive};

use crate::pattern::Pattern;
use roc_collections::all::SendMap;
use roc_module::ident::TagName;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};

/// We use a rust macro to ensure that every LowLevel gets handled
macro_rules! map_symbol_to_lowlevel_and_arity {
    ($($lowlevel:ident; $symbol:ident; $number_of_args:literal),* $(,)?) => {
        fn def_for_symbol(symbol: Symbol, var_store: &mut VarStore) -> Option<Def> {
            // expands to a big (but non-exhaustive) match on symbols and maps them to a def
            // usually this means wrapping a lowlevel in a `Def` with the right number of
            // arguments (see the big enumeration below). In this match we have a bunch of cases
            // where that default strategy does not work.
            match symbol {
                $(
                Symbol::$symbol => Some((lowlevel_n($number_of_args))(Symbol::$symbol, LowLevel::$lowlevel, var_store)),
                )*

                Symbol::NUM_TO_I8 => Some(lowlevel_1(Symbol::NUM_TO_I8, LowLevel::NumIntCast, var_store)),
                Symbol::NUM_TO_I16 => Some(lowlevel_1(Symbol::NUM_TO_I16, LowLevel::NumIntCast, var_store)),
                Symbol::NUM_TO_I32 => Some(lowlevel_1(Symbol::NUM_TO_I32, LowLevel::NumIntCast, var_store)),
                Symbol::NUM_TO_I64 => Some(lowlevel_1(Symbol::NUM_TO_I64, LowLevel::NumIntCast, var_store)),
                Symbol::NUM_TO_I128 => Some(lowlevel_1(Symbol::NUM_TO_I128, LowLevel::NumIntCast, var_store)),
                Symbol::NUM_TO_U8 => Some(lowlevel_1(Symbol::NUM_TO_U8, LowLevel::NumIntCast, var_store)),
                Symbol::NUM_TO_U16 => Some(lowlevel_1(Symbol::NUM_TO_U16, LowLevel::NumIntCast, var_store)),
                Symbol::NUM_TO_U32 => Some(lowlevel_1(Symbol::NUM_TO_U32, LowLevel::NumIntCast, var_store)),
                Symbol::NUM_TO_U64 => Some(lowlevel_1(Symbol::NUM_TO_U64, LowLevel::NumIntCast, var_store)),
                Symbol::NUM_TO_U128 => Some(lowlevel_1(Symbol::NUM_TO_U128, LowLevel::NumIntCast, var_store)),

                Symbol::NUM_INT_CAST => Some(lowlevel_1(Symbol::NUM_INT_CAST, LowLevel::NumIntCast, var_store)),

                Symbol::NUM_TO_F32 => Some(lowlevel_1(Symbol::NUM_TO_F32, LowLevel::NumToFloatCast, var_store)),
                Symbol::NUM_TO_F64 => Some(lowlevel_1(Symbol::NUM_TO_F64, LowLevel::NumToFloatCast, var_store)),

                Symbol::NUM_TO_I8_CHECKED => Some(to_num_checked(Symbol::NUM_TO_I8_CHECKED, var_store, LowLevel::NumToIntChecked)),
                Symbol::NUM_TO_I16_CHECKED => Some(to_num_checked(Symbol::NUM_TO_I16_CHECKED, var_store, LowLevel::NumToIntChecked)),
                Symbol::NUM_TO_I32_CHECKED => Some(to_num_checked(Symbol::NUM_TO_I32_CHECKED, var_store, LowLevel::NumToIntChecked)),
                Symbol::NUM_TO_I64_CHECKED => Some(to_num_checked(Symbol::NUM_TO_I64_CHECKED, var_store, LowLevel::NumToIntChecked)),
                Symbol::NUM_TO_I128_CHECKED => Some(to_num_checked(Symbol::NUM_TO_I128_CHECKED, var_store, LowLevel::NumToIntChecked)),
                Symbol::NUM_TO_U8_CHECKED => Some(to_num_checked(Symbol::NUM_TO_U8_CHECKED, var_store, LowLevel::NumToIntChecked)),
                Symbol::NUM_TO_U16_CHECKED => Some(to_num_checked(Symbol::NUM_TO_U16_CHECKED, var_store, LowLevel::NumToIntChecked)),
                Symbol::NUM_TO_U32_CHECKED => Some(to_num_checked(Symbol::NUM_TO_U32_CHECKED, var_store, LowLevel::NumToIntChecked)),
                Symbol::NUM_TO_U64_CHECKED => Some(to_num_checked(Symbol::NUM_TO_U64_CHECKED, var_store, LowLevel::NumToIntChecked)),
                Symbol::NUM_TO_U128_CHECKED => Some(to_num_checked(Symbol::NUM_TO_U128_CHECKED, var_store, LowLevel::NumToIntChecked)),

                Symbol::NUM_TO_F32_CHECKED => Some(to_num_checked(Symbol::NUM_TO_F32_CHECKED, var_store, LowLevel::NumToFloatChecked)),
                Symbol::NUM_TO_F64_CHECKED => Some(to_num_checked(Symbol::NUM_TO_F64_CHECKED, var_store, LowLevel::NumToFloatChecked)),

                Symbol::NUM_IS_ZERO => Some(to_num_is_zero(Symbol::NUM_IS_ZERO, var_store)),

                _ => None,
            }
        }

        fn _enforce_exhaustiveness(lowlevel: LowLevel) -> Symbol {
            // when adding a new lowlevel, this match will stop being exhaustive, and give a
            // compiler error. Most likely, you are adding a new lowlevel that maps directly to a
            // symbol. For instance, you want to have `List.foo` to stand for the `ListFoo`
            // lowlevel. In that case, see below in the invocation of `map_symbol_to_lowlevel_and_arity!`
            //
            // Below, we explicitly handle some exceptions to the pattern where a lowlevel maps
            // directly to a symbol. If you are unsure if your lowlevel is an exception, assume
            // that it isn't and just see if that works.
            #[allow(unreachable_patterns)] // multiple symbols can map to one low-level
            match lowlevel {
                $(
                LowLevel::$lowlevel => Symbol::$symbol,
                )*

                // these are implemented explicitly in for_symbol because they are polymorphic
                LowLevel::NumIntCast => unreachable!(),
                LowLevel::NumToFloatCast => unreachable!(),
                LowLevel::NumToIntChecked => unreachable!(),
                LowLevel::NumToFloatChecked => unreachable!(),

                // these are used internally and not tied to a symbol
                LowLevel::Hash => unimplemented!(),
                LowLevel::PtrCast => unimplemented!(),
                LowLevel::PtrStore => unimplemented!(),
                LowLevel::PtrLoad => unimplemented!(),
                LowLevel::PtrClearTagId => unimplemented!(),
                LowLevel::RefCountIncRcPtr => unimplemented!(),
                LowLevel::RefCountDecRcPtr=> unimplemented!(),
                LowLevel::RefCountIncDataPtr => unimplemented!(),
                LowLevel::RefCountDecDataPtr=> unimplemented!(),
                LowLevel::RefCountIsUnique => unimplemented!(),
                LowLevel::ListIncref => unimplemented!(),
                LowLevel::ListDecref => unimplemented!(),

                LowLevel::SetJmp => unimplemented!(),
                LowLevel::LongJmp => unimplemented!(),
                LowLevel::SetLongJmpBuffer => unimplemented!(),

                // these are not implemented, not sure why
                LowLevel::StrFromInt => unimplemented!(),
                LowLevel::StrFromFloat => unimplemented!(),
                LowLevel::NumIsFinite => unimplemented!(),
            }
        }
    };
}

// here is where we actually specify the mapping for the fast majority of cases that follow the
// pattern of a symbol mapping directly to a lowlevel. In other words, most lowlevels (left) are generated
// by only one specific symbol (center). We also specify the arity (number of arguments) of the lowlevel (right)
map_symbol_to_lowlevel_and_arity! {
    StrConcat; STR_CONCAT; 2,
    StrJoinWith; STR_JOIN_WITH; 2,
    StrIsEmpty; STR_IS_EMPTY; 1,
    StrStartsWith; STR_STARTS_WITH; 2,
    StrEndsWith; STR_ENDS_WITH; 2,
    StrSplitOn; STR_SPLIT_ON; 2,
    StrCountUtf8Bytes; STR_COUNT_UTF8_BYTES; 1,
    StrFromUtf8; STR_FROM_UTF8_LOWLEVEL; 1,
    StrToUtf8; STR_TO_UTF8; 1,
    StrRepeat; STR_REPEAT; 2,
    StrTrim; STR_TRIM; 1,
    StrTrimStart; STR_TRIM_START; 1,
    StrTrimEnd; STR_TRIM_END; 1,
    StrGetUnsafe; STR_GET_UNSAFE; 2,
    StrSubstringUnsafe; STR_SUBSTRING_UNSAFE; 3,
    StrReserve; STR_RESERVE; 2,
    StrToNum; STR_TO_NUM; 1,
    StrWithCapacity; STR_WITH_CAPACITY; 1,
    StrReleaseExcessCapacity; STR_RELEASE_EXCESS_CAPACITY; 1,
    StrWithAsciiLowercased; STR_WITH_ASCII_LOWERCASED; 1,

    ListLenUsize; LIST_LEN_USIZE; 1,
    ListLenU64; LIST_LEN_U64; 1,
    ListWithCapacity; LIST_WITH_CAPACITY; 1,
    ListReserve; LIST_RESERVE; 2,
    ListIsUnique; LIST_IS_UNIQUE; 1,
    ListClone; LIST_CLONE; 1,
    ListAppendUnsafe; LIST_APPEND_UNSAFE; 2,
    ListPrepend; LIST_PREPEND; 2,
    ListGetUnsafe; LIST_GET_UNSAFE; 2,
    ListReplaceUnsafe; LIST_REPLACE_UNSAFE; 3,
    ListConcat; LIST_CONCAT; 2,
    ListSortWith; LIST_SORT_WITH; 2,
    ListSublist; LIST_SUBLIST_LOWLEVEL; 3,
    ListDropAt; LIST_DROP_AT; 2,
    ListSwap; LIST_SWAP; 3,
    ListGetCapacity; LIST_CAPACITY; 1,
    ListReleaseExcessCapacity; LIST_RELEASE_EXCESS_CAPACITY; 1,
    ListConcatUtf8; LIST_CONCAT_UTF8; 2,

    ListGetUnsafe; DICT_LIST_GET_UNSAFE; 2,

    NumAdd; NUM_ADD; 2,
    NumAddWrap; NUM_ADD_WRAP; 2,
    NumAddChecked; NUM_ADD_CHECKED_LOWLEVEL; 2,
    NumAddSaturated; NUM_ADD_SATURATED; 2,
    NumSub; NUM_SUB; 2,
    NumSubWrap; NUM_SUB_WRAP; 2,
    NumSubChecked; NUM_SUB_CHECKED_LOWLEVEL; 2,
    NumSubSaturated; NUM_SUB_SATURATED; 2,
    NumMul; NUM_MUL; 2,
    NumMulWrap; NUM_MUL_WRAP; 2,
    NumMulSaturated; NUM_MUL_SATURATED; 2,
    NumMulChecked; NUM_MUL_CHECKED_LOWLEVEL; 2,
    NumGt; NUM_GT; 2,
    NumGte; NUM_GTE; 2,
    NumLt; NUM_LT; 2,
    NumLte; NUM_LTE; 2,
    NumCompare; NUM_COMPARE; 2,
    NumDivFrac; NUM_DIV_FRAC; 2,
    NumDivTruncUnchecked; NUM_DIV_TRUNC_UNCHECKED; 2,
    NumDivCeilUnchecked; NUM_DIV_CEIL; 2,
    NumRemUnchecked; NUM_REM_UNCHECKED; 2,
    NumIsMultipleOf; NUM_IS_MULTIPLE_OF; 2,
    NumAbs; NUM_ABS; 1,
    NumNeg; NUM_NEG; 1,
    NumSin; NUM_SIN; 1,
    NumCos; NUM_COS; 1,
    NumTan; NUM_TAN; 1,
    NumSqrtUnchecked; NUM_SQRT; 1,
    NumLogUnchecked; NUM_LOG; 1,
    NumRound; NUM_ROUND; 1,
    NumToFrac; NUM_TO_FRAC; 1,
    NumIsNan; NUM_IS_NAN; 1,
    NumIsInfinite; NUM_IS_INFINITE; 1,
    NumIsFinite; NUM_IS_FINITE; 1,
    NumPow; NUM_POW; 2,
    NumCeiling; NUM_CEILING; 1,
    NumPowInt; NUM_POW_INT; 2,
    NumFloor; NUM_FLOOR; 1,
    NumAtan; NUM_ATAN; 1,
    NumAcos; NUM_ACOS; 1,
    NumAsin; NUM_ASIN; 1,
    NumBitwiseAnd; NUM_BITWISE_AND; 2,
    NumBitwiseXor; NUM_BITWISE_XOR; 2,
    NumBitwiseOr; NUM_BITWISE_OR; 2,
    NumShiftLeftBy; NUM_SHIFT_LEFT; 2,
    NumShiftRightBy; NUM_SHIFT_RIGHT; 2,
    NumShiftRightZfBy; NUM_SHIFT_RIGHT_ZERO_FILL; 2,
    NumToStr; NUM_TO_STR; 1,
    NumCountLeadingZeroBits; NUM_COUNT_LEADING_ZERO_BITS; 1,
    NumCountTrailingZeroBits; NUM_COUNT_TRAILING_ZERO_BITS; 1,
    NumCountOneBits; NUM_COUNT_ONE_BITS; 1,
    NumWithoutDecimalPoint; NUM_WITHOUT_DECIMAL_POINT; 1,
    NumWithDecimalPoint; NUM_WITH_DECIMAL_POINT; 1,
    NumF32ToParts; NUM_F32_TO_PARTS; 1,
    NumF64ToParts; NUM_F64_TO_PARTS; 1,
    NumF32FromParts; NUM_F32_FROM_PARTS; 1,
    NumF64FromParts; NUM_F64_FROM_PARTS; 1,

    Eq; BOOL_STRUCTURAL_EQ; 2,
    NotEq; BOOL_STRUCTURAL_NOT_EQ; 2,
    Not; BOOL_NOT; 1,
    BoxExpr; BOX_BOX_FUNCTION; 1,
    UnboxExpr; BOX_UNBOX; 1,
    Unreachable; LIST_UNREACHABLE; 1,
    DictPseudoSeed; DICT_PSEUDO_SEED; 1,
}

/// Some builtins cannot be constructed in code gen alone, and need to be defined
/// as separate Roc defs. For example, List.get has this type:
///
/// List.get : List elem, U64 -> Result elem [OutOfBounds]*
///
/// Because this returns an open tag union for its Err type, it's not possible
/// for code gen to return a hardcoded value for OutOfBounds. For example,
/// if this Result unifies to [Foo, OutOfBounds] then OutOfBOunds will
/// get assigned the number 1 (because Foo got 0 alphabetically), whereas
/// if it unifies to [OutOfBounds, Qux] then OutOfBounds will get the number 0.
///
/// Getting these numbers right requires having List.get participate in the
/// normal type-checking and monomorphization processes. As such, this function
/// returns a normal def for List.get, which performs a bounds check and then
/// delegates to the compiler-internal List.getUnsafe function to do the actual
/// lookup (if the bounds check passed). That internal function is hardcoded in code gen,
/// which works fine because it doesn't involve any open tag unions.

/// Implementation for a builtin
pub fn builtin_defs_map(symbol: Symbol, var_store: &mut VarStore) -> Option<Def> {
    debug_assert!(symbol.is_builtin());

    def_for_symbol(symbol, var_store)
}

fn lowlevel_n(n: usize) -> fn(Symbol, LowLevel, &mut VarStore) -> Def {
    match n {
        0 => unimplemented!(),
        1 => lowlevel_1,
        2 => lowlevel_2,
        3 => lowlevel_3,
        4 => lowlevel_4,
        5 => lowlevel_5,
        _ => unimplemented!(),
    }
}

fn lowlevel_1(symbol: Symbol, op: LowLevel, var_store: &mut VarStore) -> Def {
    let arg1_var = var_store.fresh();
    let ret_var = var_store.fresh();

    let body = RunLowLevel {
        op,
        args: vec![(arg1_var, Var(Symbol::ARG_1, arg1_var))],
        ret_var,
    };

    defn(
        symbol,
        vec![(arg1_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

fn lowlevel_2(symbol: Symbol, op: LowLevel, var_store: &mut VarStore) -> Def {
    let arg1_var = var_store.fresh();
    let arg2_var = var_store.fresh();
    let ret_var = var_store.fresh();

    let body = RunLowLevel {
        op,
        args: vec![
            (arg1_var, Var(Symbol::ARG_1, arg1_var)),
            (arg2_var, Var(Symbol::ARG_2, arg2_var)),
        ],
        ret_var,
    };

    defn(
        symbol,
        vec![(arg1_var, Symbol::ARG_1), (arg2_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
}

fn lowlevel_3(symbol: Symbol, op: LowLevel, var_store: &mut VarStore) -> Def {
    let arg1_var = var_store.fresh();
    let arg2_var = var_store.fresh();
    let arg3_var = var_store.fresh();
    let ret_var = var_store.fresh();

    let body = RunLowLevel {
        op,
        args: vec![
            (arg1_var, Var(Symbol::ARG_1, arg1_var)),
            (arg2_var, Var(Symbol::ARG_2, arg2_var)),
            (arg3_var, Var(Symbol::ARG_3, arg3_var)),
        ],
        ret_var,
    };

    defn(
        symbol,
        vec![
            (arg1_var, Symbol::ARG_1),
            (arg2_var, Symbol::ARG_2),
            (arg3_var, Symbol::ARG_3),
        ],
        var_store,
        body,
        ret_var,
    )
}

fn lowlevel_4(symbol: Symbol, op: LowLevel, var_store: &mut VarStore) -> Def {
    let arg1_var = var_store.fresh();
    let arg2_var = var_store.fresh();
    let arg3_var = var_store.fresh();
    let arg4_var = var_store.fresh();
    let ret_var = var_store.fresh();

    let body = RunLowLevel {
        op,
        args: vec![
            (arg1_var, Var(Symbol::ARG_1, arg1_var)),
            (arg2_var, Var(Symbol::ARG_2, arg2_var)),
            (arg3_var, Var(Symbol::ARG_3, arg3_var)),
            (arg4_var, Var(Symbol::ARG_4, arg4_var)),
        ],
        ret_var,
    };

    defn(
        symbol,
        vec![
            (arg1_var, Symbol::ARG_1),
            (arg2_var, Symbol::ARG_2),
            (arg3_var, Symbol::ARG_3),
            (arg4_var, Symbol::ARG_4),
        ],
        var_store,
        body,
        ret_var,
    )
}

fn lowlevel_5(symbol: Symbol, op: LowLevel, var_store: &mut VarStore) -> Def {
    let arg1_var = var_store.fresh();
    let arg2_var = var_store.fresh();
    let arg3_var = var_store.fresh();
    let arg4_var = var_store.fresh();
    let arg5_var = var_store.fresh();
    let ret_var = var_store.fresh();

    let body = RunLowLevel {
        op,
        args: vec![
            (arg1_var, Var(Symbol::ARG_1, arg1_var)),
            (arg2_var, Var(Symbol::ARG_2, arg2_var)),
            (arg3_var, Var(Symbol::ARG_3, arg3_var)),
            (arg4_var, Var(Symbol::ARG_4, arg4_var)),
            (arg5_var, Var(Symbol::ARG_5, arg5_var)),
        ],
        ret_var,
    };

    defn(
        symbol,
        vec![
            (arg1_var, Symbol::ARG_1),
            (arg2_var, Symbol::ARG_2),
            (arg3_var, Symbol::ARG_3),
            (arg4_var, Symbol::ARG_4),
            (arg5_var, Symbol::ARG_5),
        ],
        var_store,
        body,
        ret_var,
    )
}

#[inline(always)]
fn defn(
    fn_name: Symbol,
    args: Vec<(Variable, Symbol)>,
    var_store: &mut VarStore,
    body: Expr,
    ret_var: Variable,
) -> Def {
    let expr = defn_help(fn_name, args, var_store, body, ret_var);

    Def {
        loc_pattern: Loc {
            region: Region::zero(),
            value: Pattern::Identifier(fn_name),
        },
        loc_expr: Loc {
            region: Region::zero(),
            value: expr,
        },
        expr_var: var_store.fresh(),
        pattern_vars: SendMap::default(),
        annotation: None,
        kind: crate::def::DefKind::Let,
    }
}

#[inline(always)]
fn defn_help(
    fn_name: Symbol,
    args: Vec<(Variable, Symbol)>,
    var_store: &mut VarStore,
    body: Expr,
    ret_var: Variable,
) -> Expr {
    use crate::pattern::Pattern::*;

    let closure_args = args
        .into_iter()
        .map(|(var, symbol)| {
            (
                var,
                AnnotatedMark::new(var_store),
                no_region(Identifier(symbol)),
            )
        })
        .collect();

    Closure(ClosureData {
        function_type: var_store.fresh(),
        closure_type: var_store.fresh(),
        return_type: ret_var,
        fx_type: Variable::PURE,
        early_returns: vec![],
        name: fn_name,
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments: closure_args,
        loc_body: Box::new(no_region(body)),
    })
}

#[inline(always)]
fn no_region<T>(value: T) -> Loc<T> {
    Loc {
        region: Region::zero(),
        value,
    }
}

#[inline(always)]
fn tag(name: &'static str, args: Vec<Expr>, var_store: &mut VarStore) -> Expr {
    Expr::Tag {
        tag_union_var: var_store.fresh(),
        ext_var: var_store.fresh(),
        name: TagName(name.into()),
        arguments: args
            .into_iter()
            .map(|expr| (var_store.fresh(), no_region(expr)))
            .collect::<Vec<(Variable, Loc<Expr>)>>(),
    }
}

fn to_num_checked(symbol: Symbol, var_store: &mut VarStore, lowlevel: LowLevel) -> Def {
    let bool_var = var_store.fresh();
    let num_var_1 = var_store.fresh();
    let num_var_2 = var_store.fresh();
    let ret_var = var_store.fresh();
    let record_var = var_store.fresh();

    // let arg_2 = RunLowLevel NumToXXXChecked arg_1
    // if arg_2.b then
    //   Err OutOfBounds
    // else
    //   Ok arg_2.a
    //
    // "a" and "b" because the lowlevel return value looks like { converted_val: XXX, out_of_bounds: bool },
    // and codegen will sort by alignment, so "a" will be the first key, etc.

    let cont = If {
        branch_var: ret_var,
        cond_var: bool_var,
        branches: vec![(
            // if-condition
            no_region(
                // arg_2.b
                RecordAccess {
                    record_var,
                    ext_var: var_store.fresh(),
                    field: "b".into(),
                    field_var: var_store.fresh(),
                    loc_expr: Box::new(no_region(Var(Symbol::ARG_2, var_store.fresh()))),
                },
            ),
            // out of bounds!
            no_region(tag(
                "Err",
                vec![tag("OutOfBounds", Vec::new(), var_store)],
                var_store,
            )),
        )],
        final_else: Box::new(
            // all is well
            no_region(
                // Ok arg_2.a
                tag(
                    "Ok",
                    vec![
                        // arg_2.a
                        RecordAccess {
                            record_var,
                            ext_var: var_store.fresh(),
                            field: "a".into(),
                            field_var: num_var_2,
                            loc_expr: Box::new(no_region(Var(Symbol::ARG_2, var_store.fresh()))),
                        },
                    ],
                    var_store,
                ),
            ),
        ),
    };

    // arg_2 = RunLowLevel NumToXXXChecked arg_1
    let def = crate::def::Def {
        loc_pattern: no_region(Pattern::Identifier(Symbol::ARG_2)),
        loc_expr: no_region(RunLowLevel {
            op: lowlevel,
            args: vec![(num_var_1, Var(Symbol::ARG_1, var_store.fresh()))],
            ret_var: record_var,
        }),
        expr_var: record_var,
        pattern_vars: SendMap::default(),
        annotation: None,
        kind: crate::def::DefKind::Let,
    };

    let body = LetNonRec(Box::new(def), Box::new(no_region(cont)));

    defn(
        symbol,
        vec![(num_var_1, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

fn to_num_is_zero(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let num_var = var_store.fresh();

    let body = Expr::RunLowLevel {
        op: LowLevel::Eq,
        args: vec![
            (num_var, Var(Symbol::ARG_1, num_var)),
            (
                num_var,
                Num(
                    var_store.fresh(),
                    "0".to_string().into_boxed_str(),
                    crate::expr::IntValue::I128(0i128.to_ne_bytes()),
                    roc_types::num::NumBound::None,
                ),
            ),
        ],
        ret_var: bool_var,
    };

    defn(
        symbol,
        vec![(num_var, Symbol::ARG_1)],
        var_store,
        body,
        bool_var,
    )
}
