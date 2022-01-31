use crate::def::Def;
use crate::expr::{self, ClosureData, Expr::*};
use crate::expr::{Expr, Field, Recursive};
use crate::pattern::Pattern;
use roc_collections::all::SendMap;
use roc_module::called_via::CalledVia;
use roc_module::ident::{Lowercase, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};

macro_rules! macro_magic {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(matches!(@single $rest)),*]));

    ($symbol:expr; $var_store:expr; $($key:ident => $func:expr,)+) => { macro_magic!($symbol; $var_store; $($key => $func),+) };
    ($symbol:expr; $var_store:expr; $($key:ident => $func:expr),*) => {
        {
            match $symbol {
            $(
                Symbol::$key => Some($func(Symbol::$key, $var_store)),
            )*
                _ => None,
            }
        }
    };
}

/// Some builtins cannot be constructed in code gen alone, and need to be defined
/// as separate Roc defs. For example, List.get has this type:
///
/// List.get : List elem, Nat -> Result elem [ OutOfBounds ]*
///
/// Because this returns an open tag union for its Err type, it's not possible
/// for code gen to return a hardcoded value for OutOfBounds. For example,
/// if this Result unifies to [ Foo, OutOfBounds ] then OutOfBOunds will
/// get assigned the number 1 (because Foo got 0 alphabetically), whereas
/// if it unifies to [ OutOfBounds, Qux ] then OutOfBounds will get the number 0.
///
/// Getting these numbers right requires having List.get participate in the
/// normal type-checking and monomorphization processes. As such, this function
/// returns a normal def for List.get, which performs a bounds check and then
/// delegates to the compiler-internal List.getUnsafe function to do the actual
/// lookup (if the bounds check passed). That internal function is hardcoded in code gen,
/// which works fine because it doesn't involve any open tag unions.

/// Does a builtin depend on any other builtins?
///
/// NOTE: you are supposed to give all symbols that are relied on,
/// even those that are relied on transitively!
pub fn builtin_dependencies(symbol: Symbol) -> &'static [Symbol] {
    match symbol {
        Symbol::LIST_SORT_ASC => &[Symbol::LIST_SORT_WITH, Symbol::NUM_COMPARE],
        Symbol::LIST_SORT_DESC => &[Symbol::LIST_SORT_WITH],
        Symbol::LIST_PRODUCT => &[Symbol::LIST_WALK, Symbol::NUM_MUL],
        Symbol::LIST_SUM => &[Symbol::LIST_WALK, Symbol::NUM_ADD],
        Symbol::LIST_JOIN_MAP => &[Symbol::LIST_WALK, Symbol::LIST_CONCAT],
        _ => &[],
    }
}

/// Implementation for a builtin
pub fn builtin_defs_map(symbol: Symbol, var_store: &mut VarStore) -> Option<Def> {
    debug_assert!(symbol.is_builtin());

    macro_magic! { symbol; var_store;
        BOOL_EQ => bool_eq,
        BOOL_NEQ => bool_neq,
        BOOL_AND => bool_and,
        BOOL_OR => bool_or,
        BOOL_NOT => bool_not,
        STR_CONCAT => str_concat,
        STR_JOIN_WITH => str_join_with,
        STR_SPLIT => str_split,
        STR_IS_EMPTY => str_is_empty,
        STR_STARTS_WITH => str_starts_with,
        STR_STARTS_WITH_CODE_PT => str_starts_with_code_point,
        STR_ENDS_WITH => str_ends_with,
        STR_COUNT_GRAPHEMES => str_count_graphemes,
        STR_FROM_UTF8 => str_from_utf8,
        STR_FROM_UTF8_RANGE => str_from_utf8_range,
        STR_TO_UTF8 => str_to_utf8,
        STR_REPEAT => str_repeat,
        STR_TRIM => str_trim,
        STR_TRIM_LEFT => str_trim_left,
        STR_TRIM_RIGHT => str_trim_right,
        STR_TO_DEC => str_to_num,
        STR_TO_F64 => str_to_num,
        STR_TO_F32 => str_to_num,
        STR_TO_NAT => str_to_num,
        STR_TO_U128 => str_to_num,
        STR_TO_I128 => str_to_num,
        STR_TO_U64 => str_to_num,
        STR_TO_I64 => str_to_num,
        STR_TO_U32 => str_to_num,
        STR_TO_I32 => str_to_num,
        STR_TO_U16 => str_to_num,
        STR_TO_I16 => str_to_num,
        STR_TO_U8 => str_to_num,
        STR_TO_I8 => str_to_num,
        LIST_LEN => list_len,
        LIST_GET => list_get,
        LIST_SET => list_set,
        LIST_APPEND => list_append,
        LIST_FIRST => list_first,
        LIST_LAST => list_last,
        LIST_IS_EMPTY => list_is_empty,
        LIST_SINGLE => list_single,
        LIST_REPEAT => list_repeat,
        LIST_REVERSE => list_reverse,
        LIST_CONCAT => list_concat,
        LIST_CONTAINS => list_contains,
        LIST_MIN => list_min,
        LIST_MAX => list_max,
        LIST_SUM => list_sum,
        LIST_PRODUCT => list_product,
        LIST_PREPEND => list_prepend,
        LIST_JOIN => list_join,
        LIST_JOIN_MAP => list_join_map,
        LIST_MAP => list_map,
        LIST_MAP2 => list_map2,
        LIST_MAP3 => list_map3,
        LIST_MAP4 => list_map4,
        LIST_TAKE_FIRST => list_take_first,
        LIST_TAKE_LAST => list_take_last,
        LIST_SUBLIST => list_sublist,
        LIST_SPLIT => list_split,
        LIST_INTERSPERSE => list_intersperse,
        LIST_DROP => list_drop,
        LIST_DROP_AT => list_drop_at,
        LIST_DROP_FIRST => list_drop_first,
        LIST_DROP_IF => list_drop_if,
        LIST_DROP_LAST => list_drop_last,
        LIST_SWAP => list_swap,
        LIST_MAP_WITH_INDEX => list_map_with_index,
        LIST_KEEP_IF => list_keep_if,
        LIST_KEEP_OKS => list_keep_oks,
        LIST_KEEP_ERRS=> list_keep_errs,
        LIST_RANGE => list_range,
        LIST_WALK => list_walk,
        LIST_WALK_BACKWARDS => list_walk_backwards,
        LIST_WALK_UNTIL => list_walk_until,
        LIST_SORT_WITH => list_sort_with,
        LIST_SORT_ASC => list_sort_asc,
        LIST_SORT_DESC => list_sort_desc,
        LIST_ANY => list_any,
        LIST_ALL => list_all,
        LIST_FIND => list_find,
        DICT_LEN => dict_len,
        DICT_EMPTY => dict_empty,
        DICT_SINGLE => dict_single,
        DICT_INSERT => dict_insert,
        DICT_REMOVE => dict_remove,
        DICT_GET => dict_get,
        DICT_CONTAINS => dict_contains,
        DICT_KEYS => dict_keys,
        DICT_VALUES => dict_values,
        DICT_UNION=> dict_union,
        DICT_INTERSECTION=> dict_intersection,
        DICT_DIFFERENCE=> dict_difference,
        DICT_WALK=> dict_walk,
        SET_EMPTY => set_empty,
        SET_LEN => set_len,
        SET_SINGLE => set_single,
        SET_UNION=> set_union,
        SET_INTERSECTION => set_intersection,
        SET_DIFFERENCE => set_difference,
        SET_TO_LIST => set_to_list,
        SET_FROM_LIST => set_from_list,
        SET_INSERT => set_insert,
        SET_REMOVE => set_remove,
        SET_CONTAINS => set_contains,
        SET_WALK=> set_walk,
        NUM_ADD => num_add,
        NUM_ADD_CHECKED => num_add_checked,
        NUM_ADD_WRAP => num_add_wrap,
        NUM_ADD_SATURATED => num_add_saturated,
        NUM_SUB => num_sub,
        NUM_SUB_WRAP => num_sub_wrap,
        NUM_SUB_CHECKED => num_sub_checked,
        NUM_SUB_SATURATED => num_sub_saturated,
        NUM_MUL => num_mul,
        NUM_MUL_WRAP => num_mul_wrap,
        NUM_MUL_CHECKED => num_mul_checked,
        NUM_GT => num_gt,
        NUM_GTE => num_gte,
        NUM_LT => num_lt,
        NUM_LTE => num_lte,
        NUM_COMPARE => num_compare,
        NUM_SIN => num_sin,
        NUM_COS => num_cos,
        NUM_TAN => num_tan,
        NUM_DIV_FLOAT => num_div_float,
        NUM_DIV_INT => num_div_int,
        NUM_DIV_CEIL => num_div_ceil,
        NUM_ABS => num_abs,
        NUM_NEG => num_neg,
        NUM_REM => num_rem,
        NUM_IS_MULTIPLE_OF => num_is_multiple_of,
        NUM_SQRT => num_sqrt,
        NUM_LOG => num_log,
        NUM_ROUND => num_round,
        NUM_IS_ODD => num_is_odd,
        NUM_IS_EVEN => num_is_even,
        NUM_IS_ZERO => num_is_zero,
        NUM_IS_POSITIVE => num_is_positive,
        NUM_IS_NEGATIVE => num_is_negative,
        NUM_TO_FLOAT => num_to_float,
        NUM_POW => num_pow,
        NUM_CEILING => num_ceiling,
        NUM_POW_INT => num_pow_int,
        NUM_FLOOR => num_floor,
        NUM_ATAN => num_atan,
        NUM_ACOS => num_acos,
        NUM_ASIN => num_asin,
        NUM_BYTES_TO_U16 => num_bytes_to_u16,
        NUM_BYTES_TO_U32 => num_bytes_to_u32,
        NUM_BITWISE_AND => num_bitwise_and,
        NUM_BITWISE_XOR => num_bitwise_xor,
        NUM_BITWISE_OR => num_bitwise_or,
        NUM_SHIFT_LEFT=> num_shift_left_by,
        NUM_SHIFT_RIGHT => num_shift_right_by,
        NUM_SHIFT_RIGHT_ZERO_FILL => num_shift_right_zf_by,
        NUM_INT_CAST=> num_int_cast,
        NUM_MIN_I8=> num_min_i8,
        NUM_MAX_I8=> num_max_i8,
        NUM_MIN_U8=> num_min_u8,
        NUM_MAX_U8=> num_max_u8,
        NUM_MIN_I16=> num_min_i16,
        NUM_MAX_I16=> num_max_i16,
        NUM_MIN_U16=> num_min_u16,
        NUM_MAX_U16=> num_max_u16,
        NUM_MIN_I32=> num_min_i32,
        NUM_MAX_I32=> num_max_i32,
        NUM_MIN_U32=> num_min_u32,
        NUM_MAX_U32=> num_max_u32,
        NUM_MIN_I64=> num_min_i64,
        NUM_MAX_I64=> num_max_i64,
        NUM_MIN_U64=> num_min_u64,
        NUM_MAX_U64=> num_max_u64,
        NUM_MIN_I128=> num_min_i128,
        NUM_MAX_I128=> num_max_i128,
        NUM_TO_STR => num_to_str,
        RESULT_MAP => result_map,
        RESULT_MAP_ERR => result_map_err,
        RESULT_AFTER => result_after,
        RESULT_WITH_DEFAULT => result_with_default,
        RESULT_IS_OK => result_is_ok,
        RESULT_IS_ERR => result_is_err,
    }
}

fn lowlevel_1(symbol: Symbol, op: LowLevel, var_store: &mut VarStore) -> Def {
    let arg1_var = var_store.fresh();
    let ret_var = var_store.fresh();

    let body = RunLowLevel {
        op,
        args: vec![(arg1_var, Var(Symbol::ARG_1))],
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
            (arg1_var, Var(Symbol::ARG_1)),
            (arg2_var, Var(Symbol::ARG_2)),
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
            (arg1_var, Var(Symbol::ARG_1)),
            (arg2_var, Var(Symbol::ARG_2)),
            (arg3_var, Var(Symbol::ARG_3)),
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
            (arg1_var, Var(Symbol::ARG_1)),
            (arg2_var, Var(Symbol::ARG_2)),
            (arg3_var, Var(Symbol::ARG_3)),
            (arg4_var, Var(Symbol::ARG_4)),
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
            (arg1_var, Var(Symbol::ARG_1)),
            (arg2_var, Var(Symbol::ARG_2)),
            (arg3_var, Var(Symbol::ARG_3)),
            (arg4_var, Var(Symbol::ARG_4)),
            (arg5_var, Var(Symbol::ARG_5)),
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

// Num.toStr : Num a -> Str
fn num_to_str(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let num_var = var_store.fresh();
    let str_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumToStr,
        args: vec![(num_var, Var(Symbol::ARG_1))],
        ret_var: str_var,
    };

    defn(
        symbol,
        vec![(num_var, Symbol::ARG_1)],
        var_store,
        body,
        str_var,
    )
}

/// Bool.isEq : val, val -> Bool
fn bool_eq(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::Eq,
        args: vec![(arg_var, Var(Symbol::ARG_1)), (arg_var, Var(Symbol::ARG_2))],
        ret_var: bool_var,
    };

    defn(
        symbol,
        vec![(arg_var, Symbol::ARG_1), (arg_var, Symbol::ARG_2)],
        var_store,
        body,
        bool_var,
    )
}

/// Bool.isNotEq : val, val -> Bool
fn bool_neq(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::NotEq,
        args: vec![(arg_var, Var(Symbol::ARG_1)), (arg_var, Var(Symbol::ARG_2))],
        ret_var: bool_var,
    };

    defn(
        symbol,
        vec![(arg_var, Symbol::ARG_1), (arg_var, Symbol::ARG_2)],
        var_store,
        body,
        bool_var,
    )
}

/// Bool.or : Bool, Bool -> Bool
fn bool_or(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::Or,
        args: vec![
            (bool_var, Var(Symbol::ARG_1)),
            (bool_var, Var(Symbol::ARG_2)),
        ],
        ret_var: bool_var,
    };

    defn(
        symbol,
        vec![(bool_var, Symbol::ARG_1), (bool_var, Symbol::ARG_2)],
        var_store,
        body,
        bool_var,
    )
}

/// Bool.not : Bool -> Bool
fn bool_not(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::Not,
        args: vec![(bool_var, Var(Symbol::ARG_1))],
        ret_var: bool_var,
    };

    defn(
        symbol,
        vec![(bool_var, Symbol::ARG_1)],
        var_store,
        body,
        bool_var,
    )
}

/// Bool.and : Bool, Bool -> Bool
fn bool_and(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::And,
        args: vec![
            (bool_var, Var(Symbol::ARG_1)),
            (bool_var, Var(Symbol::ARG_2)),
        ],
        ret_var: var_store.fresh(),
    };

    defn(
        symbol,
        vec![(bool_var, Symbol::ARG_1), (bool_var, Symbol::ARG_2)],
        var_store,
        body,
        bool_var,
    )
}

/// Num a, Num a -> Num a
fn num_binop(symbol: Symbol, var_store: &mut VarStore, op: LowLevel) -> Def {
    let num_var = var_store.fresh();
    let body = RunLowLevel {
        op,
        args: vec![(num_var, Var(Symbol::ARG_1)), (num_var, Var(Symbol::ARG_2))],
        ret_var: num_var,
    };

    defn(
        symbol,
        vec![(num_var, Symbol::ARG_1), (num_var, Symbol::ARG_2)],
        var_store,
        body,
        num_var,
    )
}

/// Num a, Num a -> b
fn num_num_other_binop(symbol: Symbol, var_store: &mut VarStore, op: LowLevel) -> Def {
    let num_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let body = RunLowLevel {
        op,
        args: vec![(num_var, Var(Symbol::ARG_1)), (num_var, Var(Symbol::ARG_2))],
        ret_var: bool_var,
    };

    defn(
        symbol,
        vec![(num_var, Symbol::ARG_1), (num_var, Symbol::ARG_2)],
        var_store,
        body,
        bool_var,
    )
}

/// Num.add : Num a, Num a -> Num a
fn num_add(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumAdd)
}

/// Num.addWrap : Int a, Int a -> Int a
fn num_add_wrap(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumAddWrap)
}

fn num_overflow_checked(symbol: Symbol, var_store: &mut VarStore, lowlevel: LowLevel) -> Def {
    let bool_var = var_store.fresh();
    let num_var_1 = var_store.fresh();
    let num_var_2 = var_store.fresh();
    let num_var_3 = var_store.fresh();
    let ret_var = var_store.fresh();
    let record_var = var_store.fresh();

    // let arg_3 = RunLowLevel NumXXXChecked arg_1 arg_2
    //
    // if arg_3.b then
    //  # overflow
    //  Err Overflow
    // else
    //  # all is well
    //  Ok arg_3.a

    let cont = If {
        branch_var: ret_var,
        cond_var: bool_var,
        branches: vec![(
            // if-condition
            no_region(
                // arg_3.b
                Access {
                    record_var,
                    ext_var: var_store.fresh(),
                    field: "b".into(),
                    field_var: var_store.fresh(),
                    loc_expr: Box::new(no_region(Var(Symbol::ARG_3))),
                },
            ),
            // overflow!
            no_region(tag(
                "Err",
                vec![tag("Overflow", Vec::new(), var_store)],
                var_store,
            )),
        )],
        final_else: Box::new(
            // all is well
            no_region(
                // Ok arg_3.a
                tag(
                    "Ok",
                    vec![
                        // arg_3.a
                        Access {
                            record_var,
                            ext_var: var_store.fresh(),
                            field: "a".into(),
                            field_var: num_var_3,
                            loc_expr: Box::new(no_region(Var(Symbol::ARG_3))),
                        },
                    ],
                    var_store,
                ),
            ),
        ),
    };

    // arg_3 = RunLowLevel NumXXXChecked arg_1 arg_2
    let def = crate::def::Def {
        loc_pattern: no_region(Pattern::Identifier(Symbol::ARG_3)),
        loc_expr: no_region(RunLowLevel {
            op: lowlevel,
            args: vec![
                (num_var_1, Var(Symbol::ARG_1)),
                (num_var_2, Var(Symbol::ARG_2)),
            ],
            ret_var: record_var,
        }),
        expr_var: record_var,
        pattern_vars: SendMap::default(),
        annotation: None,
    };

    let body = LetNonRec(Box::new(def), Box::new(no_region(cont)), ret_var);

    defn(
        symbol,
        vec![(num_var_1, Symbol::ARG_1), (num_var_2, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
}

/// Num.addChecked : Num a, Num a -> Result (Num a) [ Overflow ]*
fn num_add_checked(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_overflow_checked(symbol, var_store, LowLevel::NumAddChecked)
}

/// Num.addSaturated : Int a, Int a -> Int a
fn num_add_saturated(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumAddSaturated)
}

/// Num.sub : Num a, Num a -> Num a
fn num_sub(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumSub)
}

/// Num.subWrap : Int a, Int a -> Int a
fn num_sub_wrap(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumSubWrap)
}

/// Num.subChecked : Num a, Num a -> Result (Num a) [ Overflow ]*
fn num_sub_checked(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_overflow_checked(symbol, var_store, LowLevel::NumSubChecked)
}

/// Num.subSaturated : Int a, Int a -> Int a
fn num_sub_saturated(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumSubSaturated)
}

/// Num.mul : Num a, Num a -> Num a
fn num_mul(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumMul)
}

/// Num.mulWrap : Int a, Int a -> Int a
fn num_mul_wrap(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumMulWrap)
}

/// Num.mulChecked : Num a, Num a -> Result (Num a) [ Overflow ]*
fn num_mul_checked(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_overflow_checked(symbol, var_store, LowLevel::NumMulChecked)
}

/// Num.isGt : Num a, Num a -> Bool
fn num_gt(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_num_other_binop(symbol, var_store, LowLevel::NumGt)
}

/// Num.isGte : Num a, Num a -> Bool
fn num_gte(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_num_other_binop(symbol, var_store, LowLevel::NumGte)
}

/// Num.isLt : Num a, Num a -> Bool
fn num_lt(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_num_other_binop(symbol, var_store, LowLevel::NumLt)
}

/// Num.isLte : Num a, Num a -> Bool
fn num_lte(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_num_other_binop(symbol, var_store, LowLevel::NumLte)
}

/// Num.compare : Num a, Num a -> [ LT, EQ, GT ]
fn num_compare(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_num_other_binop(symbol, var_store, LowLevel::NumCompare)
}

/// Num.sin : Float -> Float
fn num_sin(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let float_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::NumSin,
        args: vec![(float_var, Var(Symbol::ARG_1))],
        ret_var: float_var,
    };

    defn(
        symbol,
        vec![(float_var, Symbol::ARG_1)],
        var_store,
        body,
        float_var,
    )
}

/// Num.cos : Float -> Float
fn num_cos(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let float_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::NumCos,
        args: vec![(float_var, Var(Symbol::ARG_1))],
        ret_var: float_var,
    };

    defn(
        symbol,
        vec![(float_var, Symbol::ARG_1)],
        var_store,
        body,
        float_var,
    )
}

/// Num.tan : Float -> Float
fn num_tan(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let float_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::NumDivUnchecked,
        args: vec![
            (
                float_var,
                RunLowLevel {
                    op: LowLevel::NumSin,
                    args: vec![(float_var, Var(Symbol::ARG_1))],
                    ret_var: float_var,
                },
            ),
            (
                float_var,
                RunLowLevel {
                    op: LowLevel::NumCos,
                    args: vec![(float_var, Var(Symbol::ARG_1))],
                    ret_var: float_var,
                },
            ),
        ],
        ret_var: float_var,
    };

    defn(
        symbol,
        vec![(float_var, Symbol::ARG_1)],
        var_store,
        body,
        float_var,
    )
}

/// Num.isZero : Num * -> Bool
fn num_is_zero(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let unbound_zero_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::Eq,
        args: vec![
            (arg_var, Var(Symbol::ARG_1)),
            (arg_var, num(unbound_zero_var, 0)),
        ],
        ret_var: bool_var,
    };

    defn(
        symbol,
        vec![(arg_var, Symbol::ARG_1)],
        var_store,
        body,
        bool_var,
    )
}

/// Num.isNegative : Num * -> Bool
fn num_is_negative(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let unbound_zero_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumGt,
        args: vec![
            (arg_var, num(unbound_zero_var, 0)),
            (arg_var, Var(Symbol::ARG_1)),
        ],
        ret_var: bool_var,
    };

    defn(
        symbol,
        vec![(arg_var, Symbol::ARG_1)],
        var_store,
        body,
        bool_var,
    )
}

/// Num.isPositive : Num * -> Bool
fn num_is_positive(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let unbound_zero_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumGt,
        args: vec![
            (arg_var, Var(Symbol::ARG_1)),
            (arg_var, num(unbound_zero_var, 0)),
        ],
        ret_var: bool_var,
    };

    defn(
        symbol,
        vec![(arg_var, Symbol::ARG_1)],
        var_store,
        body,
        bool_var,
    )
}

/// Num.isOdd : Num * -> Bool
fn num_is_odd(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let unbound_two_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::Eq,
        args: vec![
            (
                arg_var,
                int::<i128>(var_store.fresh(), var_store.fresh(), 1),
            ),
            (
                arg_var,
                RunLowLevel {
                    op: LowLevel::NumRemUnchecked,
                    args: vec![
                        (arg_var, Var(Symbol::ARG_1)),
                        (arg_var, num(unbound_two_var, 2)),
                    ],
                    ret_var: arg_var,
                },
            ),
        ],
        ret_var: bool_var,
    };

    defn(
        symbol,
        vec![(arg_var, Symbol::ARG_1)],
        var_store,
        body,
        bool_var,
    )
}

/// Num.isEven : Num * -> Bool
fn num_is_even(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let arg_num_var = var_store.fresh();
    let bool_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::Eq,
        args: vec![
            (arg_var, num(arg_num_var, 0)),
            (
                arg_var,
                RunLowLevel {
                    op: LowLevel::NumRemUnchecked,
                    args: vec![
                        (arg_var, Var(Symbol::ARG_1)),
                        (arg_var, num(arg_num_var, 2)),
                    ],
                    ret_var: arg_var,
                },
            ),
        ],
        ret_var: bool_var,
    };

    defn(
        symbol,
        vec![(arg_var, Symbol::ARG_1)],
        var_store,
        body,
        bool_var,
    )
}

/// Num.toFloat : Num * -> Float
fn num_to_float(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let float_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumToFloat,
        args: vec![(arg_var, Var(Symbol::ARG_1))],
        ret_var: float_var,
    };

    defn(
        symbol,
        vec![(arg_var, Symbol::ARG_1)],
        var_store,
        body,
        float_var,
    )
}

/// Num.sqrt : Float -> Result Float [ SqrtOfNegative ]*
fn num_sqrt(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let float_var = var_store.fresh();
    let unbound_zero_var = var_store.fresh();
    let precision_var = var_store.fresh();
    let ret_var = var_store.fresh();

    let body = If {
        branch_var: ret_var,
        cond_var: bool_var,
        branches: vec![(
            no_region(RunLowLevel {
                op: LowLevel::NumGte,
                args: vec![
                    (float_var, Var(Symbol::ARG_1)),
                    (float_var, float(unbound_zero_var, precision_var, 0.0)),
                ],
                ret_var: bool_var,
            }),
            no_region(tag(
                "Ok",
                vec![RunLowLevel {
                    op: LowLevel::NumSqrtUnchecked,
                    args: vec![(float_var, Var(Symbol::ARG_1))],
                    ret_var: float_var,
                }],
                var_store,
            )),
        )],
        final_else: Box::new(no_region(tag(
            "Err",
            vec![tag("SqrtOfNegative", Vec::new(), var_store)],
            var_store,
        ))),
    };

    defn(
        symbol,
        vec![(float_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

/// Num.log : Float -> Result Float [ LogNeedsPositive ]*
fn num_log(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let float_var = var_store.fresh();
    let unbound_zero_var = var_store.fresh();
    let precision_var = var_store.fresh();
    let ret_var = var_store.fresh();

    let body = If {
        branch_var: ret_var,
        cond_var: bool_var,
        branches: vec![(
            no_region(RunLowLevel {
                op: LowLevel::NumGt,
                args: vec![
                    (float_var, Var(Symbol::ARG_1)),
                    (float_var, float(unbound_zero_var, precision_var, 0.0)),
                ],
                ret_var: bool_var,
            }),
            no_region(tag(
                "Ok",
                vec![RunLowLevel {
                    op: LowLevel::NumLogUnchecked,
                    args: vec![(float_var, Var(Symbol::ARG_1))],
                    ret_var: float_var,
                }],
                var_store,
            )),
        )],
        final_else: Box::new(no_region(tag(
            "Err",
            vec![tag("LogNeedsPositive", Vec::new(), var_store)],
            var_store,
        ))),
    };

    defn(
        symbol,
        vec![(float_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

/// Num.round : Float -> Int
fn num_round(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let float_var = var_store.fresh();
    let int_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumRound,
        args: vec![(float_var, Var(Symbol::ARG_1))],
        ret_var: int_var,
    };

    defn(
        symbol,
        vec![(float_var, Symbol::ARG_1)],
        var_store,
        body,
        int_var,
    )
}

/// Num.pow : Float, Float -> Float
fn num_pow(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let float_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumPow,
        args: vec![
            (float_var, Var(Symbol::ARG_1)),
            (float_var, Var(Symbol::ARG_2)),
        ],
        ret_var: float_var,
    };

    defn(
        symbol,
        vec![(float_var, Symbol::ARG_1), (float_var, Symbol::ARG_2)],
        var_store,
        body,
        float_var,
    )
}

/// Num.ceiling : Float -> Int
fn num_ceiling(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let float_var = var_store.fresh();
    let int_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumCeiling,
        args: vec![(float_var, Var(Symbol::ARG_1))],
        ret_var: int_var,
    };

    defn(
        symbol,
        vec![(float_var, Symbol::ARG_1)],
        var_store,
        body,
        int_var,
    )
}

/// Num.powInt : Int a, Int a -> Int a
fn num_pow_int(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let int_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumPowInt,
        args: vec![(int_var, Var(Symbol::ARG_1)), (int_var, Var(Symbol::ARG_2))],
        ret_var: int_var,
    };

    defn(
        symbol,
        vec![(int_var, Symbol::ARG_1), (int_var, Symbol::ARG_2)],
        var_store,
        body,
        int_var,
    )
}

/// Num.floor : Float -> Int
fn num_floor(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let float_var = var_store.fresh();
    let int_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumFloor,
        args: vec![(float_var, Var(Symbol::ARG_1))],
        ret_var: int_var,
    };

    defn(
        symbol,
        vec![(float_var, Symbol::ARG_1)],
        var_store,
        body,
        int_var,
    )
}

/// Num.atan : Float -> Float
fn num_atan(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_float_var = var_store.fresh();
    let ret_float_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumAtan,
        args: vec![(arg_float_var, Var(Symbol::ARG_1))],
        ret_var: ret_float_var,
    };

    defn(
        symbol,
        vec![(arg_float_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_float_var,
    )
}

/// Num.acos : Float -> Float
fn num_acos(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_float_var = var_store.fresh();
    let ret_float_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumAcos,
        args: vec![(arg_float_var, Var(Symbol::ARG_1))],
        ret_var: ret_float_var,
    };

    defn(
        symbol,
        vec![(arg_float_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_float_var,
    )
}

/// Num.asin : Float -> Float
fn num_asin(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_float_var = var_store.fresh();
    let ret_float_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumAsin,
        args: vec![(arg_float_var, Var(Symbol::ARG_1))],
        ret_var: ret_float_var,
    };

    defn(
        symbol,
        vec![(arg_float_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_float_var,
    )
}

/// Num.bytesToU16 : List U8, Nat -> Result U16 [ OutOfBounds ]
fn num_bytes_to_u16(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_bytes_to(symbol, var_store, 1, LowLevel::NumBytesToU16)
}

/// Num.bytesToU32 : List U8, Nat -> Result U32 [ OutOfBounds ]
fn num_bytes_to_u32(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_bytes_to(symbol, var_store, 3, LowLevel::NumBytesToU32)
}

/// Num.bitwiseAnd : Int a, Int a -> Int a
fn num_bitwise_and(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumBitwiseAnd)
}

/// Num.bitwiseXor : Int a, Int a -> Int a
fn num_bitwise_xor(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumBitwiseXor)
}

/// Num.bitwiseOr: Int a, Int a -> Int a
fn num_bitwise_or(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumBitwiseOr)
}

/// Num.shiftLeftBy: Nat, Int a -> Int a
fn num_shift_left_by(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::NumShiftLeftBy, var_store)
}

/// Num.shiftRightBy: Nat, Int a -> Int a
fn num_shift_right_by(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::NumShiftRightBy, var_store)
}

/// Num.shiftRightZfBy: Nat, Int a -> Int a
fn num_shift_right_zf_by(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::NumShiftRightZfBy, var_store)
}

/// Num.intCast: Int a -> Int b
fn num_int_cast(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::NumIntCast, var_store)
}

/// Num.minI8: I8
fn num_min_i8(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<i8>(symbol, var_store, i8::MIN)
}

/// Num.maxI8: I8
fn num_max_i8(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<i8>(symbol, var_store, i8::MAX)
}

/// Num.minU8: U8
fn num_min_u8(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<u8>(symbol, var_store, u8::MIN)
}

/// Num.maxU8: U8
fn num_max_u8(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<u8>(symbol, var_store, u8::MAX)
}

/// Num.minI16: I16
fn num_min_i16(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<i16>(symbol, var_store, i16::MIN)
}

/// Num.maxI16: I16
fn num_max_i16(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<i16>(symbol, var_store, i16::MAX)
}

/// Num.minU16: U16
fn num_min_u16(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<u16>(symbol, var_store, u16::MIN)
}

/// Num.maxU16: U16
fn num_max_u16(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<u16>(symbol, var_store, u16::MAX)
}

/// Num.minI32: I32
fn num_min_i32(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<i32>(symbol, var_store, i32::MIN)
}

/// Num.maxI32: I32
fn num_max_i32(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<i32>(symbol, var_store, i32::MAX)
}

/// Num.minU32: U32
fn num_min_u32(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<u32>(symbol, var_store, u32::MIN)
}

/// Num.maxU32: U32
fn num_max_u32(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<u32>(symbol, var_store, u32::MAX)
}

/// Num.minI64: I64
fn num_min_i64(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<i64>(symbol, var_store, i64::MIN)
}

/// Num.maxI64: I64
fn num_max_i64(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<i64>(symbol, var_store, i64::MAX)
}

/// Num.minU64: U64
fn num_min_u64(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<u64>(symbol, var_store, u64::MIN)
}

/// Num.maxU64: U64
fn num_max_u64(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<u64>(symbol, var_store, u64::MAX)
}

/// Num.minI128: I128
fn num_min_i128(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<i128>(symbol, var_store, i128::MIN)
}

/// Num.maxI128: I128
fn num_max_i128(symbol: Symbol, var_store: &mut VarStore) -> Def {
    int_min_or_max::<i128>(symbol, var_store, i128::MAX)
}

/// List.isEmpty : List * -> Bool
fn list_is_empty(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let len_var = Variable::NAT;
    let unbound_zero_var = Variable::NATURAL;

    let body = RunLowLevel {
        op: LowLevel::Eq,
        args: vec![
            (len_var, num(unbound_zero_var, 0)),
            (
                len_var,
                RunLowLevel {
                    op: LowLevel::ListLen,
                    args: vec![(list_var, Var(Symbol::ARG_1))],
                    ret_var: len_var,
                },
            ),
        ],
        ret_var: bool_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1)],
        var_store,
        body,
        bool_var,
    )
}

/// List.reverse : List elem -> List elem
fn list_reverse(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListReverse,
        args: vec![(list_var, Var(Symbol::ARG_1))],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1)],
        var_store,
        body,
        list_var,
    )
}

/// Str.split : Str, Str -> List Str
fn str_split(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let str_var = var_store.fresh();
    let ret_list_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::StrSplit,
        args: vec![(str_var, Var(Symbol::ARG_1)), (str_var, Var(Symbol::ARG_2))],
        ret_var: ret_list_var,
    };

    defn(
        symbol,
        vec![(str_var, Symbol::ARG_1), (str_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_list_var,
    )
}

/// Str.trim : Str -> Str
fn str_trim(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::StrTrim, var_store)
}

/// Str.trimLeft : Str -> Str
fn str_trim_left(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::StrTrimLeft, var_store)
}

/// Str.trimRight : Str -> Str
fn str_trim_right(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::StrTrimRight, var_store)
}

/// Str.toNum : Str -> Result (Num *) [ InvalidNumStr ]*
fn str_to_num(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let str_var = var_store.fresh();
    let num_var = var_store.fresh();
    let ret_var = var_store.fresh();
    let record_var = var_store.fresh();

    let errorcode_var = var_store.fresh();

    // let arg_2 = RunLowLevel StrToNum arg_1
    //
    // if arg_2.errorcode then
    //  Err InvalidNumStr
    // else
    //  Ok arg_2.value

    let cont = If {
        branch_var: ret_var,
        cond_var: bool_var,
        branches: vec![(
            // if-condition
            no_region(RunLowLevel {
                op: LowLevel::NumGt,
                args: vec![
                    (
                        errorcode_var,
                        // arg_3.b
                        Access {
                            record_var,
                            ext_var: var_store.fresh(),
                            field: "b_errorcode".into(),
                            field_var: errorcode_var,
                            loc_expr: Box::new(no_region(Var(Symbol::ARG_2))),
                        },
                    ),
                    (
                        errorcode_var,
                        int::<i128>(errorcode_var, Variable::UNSIGNED8, 0),
                    ),
                ],
                ret_var: bool_var,
            }),
            // overflow!
            no_region(tag(
                "Err",
                vec![tag("InvalidNumStr", Vec::new(), var_store)],
                var_store,
            )),
        )],
        final_else: Box::new(
            // all is well
            no_region(
                // Ok arg_2.value
                tag(
                    "Ok",
                    vec![
                        // arg_3.a
                        Access {
                            record_var,
                            ext_var: var_store.fresh(),
                            field: "a_value".into(),
                            field_var: num_var,
                            loc_expr: Box::new(no_region(Var(Symbol::ARG_2))),
                        },
                    ],
                    var_store,
                ),
            ),
        ),
    };

    let def = crate::def::Def {
        loc_pattern: no_region(Pattern::Identifier(Symbol::ARG_2)),
        loc_expr: no_region(RunLowLevel {
            op: LowLevel::StrToNum,
            args: vec![(str_var, Var(Symbol::ARG_1))],
            ret_var: record_var,
        }),
        expr_var: record_var,
        pattern_vars: SendMap::default(),
        annotation: None,
    };

    let body = LetNonRec(Box::new(def), Box::new(no_region(cont)), ret_var);

    defn(
        symbol,
        vec![(str_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

/// Str.repeat : Str, Nat -> Str
fn str_repeat(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let str_var = var_store.fresh();
    let nat_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::StrRepeat,
        args: vec![(str_var, Var(Symbol::ARG_1)), (nat_var, Var(Symbol::ARG_2))],
        ret_var: str_var,
    };

    defn(
        symbol,
        vec![(str_var, Symbol::ARG_1), (nat_var, Symbol::ARG_2)],
        var_store,
        body,
        str_var,
    )
}

/// Str.concat : Str, Str -> Str
fn str_concat(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let str_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::StrConcat,
        args: vec![(str_var, Var(Symbol::ARG_1)), (str_var, Var(Symbol::ARG_2))],
        ret_var: str_var,
    };

    defn(
        symbol,
        vec![(str_var, Symbol::ARG_1), (str_var, Symbol::ARG_2)],
        var_store,
        body,
        str_var,
    )
}

/// Str.joinWith : List Str, Str -> Str
fn str_join_with(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_str_var = var_store.fresh();
    let str_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::StrJoinWith,
        args: vec![
            (list_str_var, Var(Symbol::ARG_1)),
            (str_var, Var(Symbol::ARG_2)),
        ],
        ret_var: str_var,
    };

    defn(
        symbol,
        vec![(list_str_var, Symbol::ARG_1), (str_var, Symbol::ARG_2)],
        var_store,
        body,
        str_var,
    )
}

/// Str.isEmpty : Str -> Bool
fn str_is_empty(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let str_var = var_store.fresh();
    let bool_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::StrIsEmpty,
        args: vec![(str_var, Var(Symbol::ARG_1))],
        ret_var: bool_var,
    };

    defn(
        symbol,
        vec![(str_var, Symbol::ARG_1)],
        var_store,
        body,
        bool_var,
    )
}

/// Str.startsWith : Str, Str -> Bool
fn str_starts_with(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::StrStartsWith, var_store)
}

/// Str.startsWithCodePt : Str, U32 -> Bool
fn str_starts_with_code_point(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::StrStartsWithCodePt, var_store)
}

/// Str.endsWith : Str, Str -> Bool
fn str_ends_with(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let str_var = var_store.fresh();
    let bool_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::StrEndsWith,
        args: vec![(str_var, Var(Symbol::ARG_1)), (str_var, Var(Symbol::ARG_2))],
        ret_var: bool_var,
    };

    defn(
        symbol,
        vec![(str_var, Symbol::ARG_1), (str_var, Symbol::ARG_2)],
        var_store,
        body,
        bool_var,
    )
}

/// Str.countGraphemes : Str -> Int
fn str_count_graphemes(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let str_var = var_store.fresh();
    let int_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::StrCountGraphemes,
        args: vec![(str_var, Var(Symbol::ARG_1))],
        ret_var: int_var,
    };

    defn(
        symbol,
        vec![(str_var, Symbol::ARG_1)],
        var_store,
        body,
        int_var,
    )
}

/// Str.fromUtf8 : List U8 -> Result Str [ BadUtf8 { byteIndex : Nat, problem : Utf8Problem  } } ]*
fn str_from_utf8(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bytes_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let record_var = var_store.fresh();
    let ret_var = var_store.fresh();

    // let arg_2 = RunLowLevel FromUtf8 arg_1
    //
    // arg_2 :
    //   { a : Bool   -- isOk
    //   , b : String -- result_str
    //   , c : Nat    -- problem_byte_index
    //   , d : I8     -- problem_code
    //   }
    //
    // if arg_2.a then
    //  # all is well
    //  Ok arg_2.str
    // else
    //  # problem
    //  Err (BadUtf8 { byteIndex: arg_2.byteIndex, problem : arg_2.problem })

    let def = crate::def::Def {
        loc_pattern: no_region(Pattern::Identifier(Symbol::ARG_2)),
        loc_expr: no_region(RunLowLevel {
            op: LowLevel::StrFromUtf8,
            args: vec![(bytes_var, Var(Symbol::ARG_1))],
            ret_var: record_var,
        }),
        expr_var: record_var,
        pattern_vars: SendMap::default(),
        annotation: None,
    };

    let cont = If {
        branch_var: ret_var,
        cond_var: bool_var,
        branches: vec![(
            // if-condition
            no_region(
                // arg_2.c -> Bool
                Access {
                    record_var,
                    ext_var: var_store.fresh(),
                    field: "c_isOk".into(),
                    field_var: var_store.fresh(),
                    loc_expr: Box::new(no_region(Var(Symbol::ARG_2))),
                },
            ),
            // all is good
            no_region(tag(
                "Ok",
                // arg_2.a -> Str
                vec![Access {
                    record_var,
                    ext_var: var_store.fresh(),
                    field: "b_str".into(),
                    field_var: var_store.fresh(),
                    loc_expr: Box::new(no_region(Var(Symbol::ARG_2))),
                }],
                var_store,
            )),
        )],
        final_else: Box::new(
            // bad!!
            no_region(tag(
                "Err",
                vec![tag(
                    "BadUtf8",
                    vec![
                        Access {
                            record_var,
                            ext_var: var_store.fresh(),
                            field: "d_problem".into(),
                            field_var: var_store.fresh(),
                            loc_expr: Box::new(no_region(Var(Symbol::ARG_2))),
                        },
                        Access {
                            record_var,
                            ext_var: var_store.fresh(),
                            field: "a_byteIndex".into(),
                            field_var: var_store.fresh(),
                            loc_expr: Box::new(no_region(Var(Symbol::ARG_2))),
                        },
                    ],
                    var_store,
                )],
                var_store,
            )),
        ),
    };

    let body = LetNonRec(Box::new(def), Box::new(no_region(cont)), ret_var);

    defn(
        symbol,
        vec![(bytes_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}
/// Str.fromUtf8Range : List U8, { start : Nat, count : Nat } -> Result Str [ BadUtf8 { byteIndex : Nat, problem : Utf8Problem  } } ]*
fn str_from_utf8_range(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bytes_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let arg_record_var = var_store.fresh();
    let ll_record_var = var_store.fresh();
    let ret_var = var_store.fresh();

    // let arg_3 = RunLowLevel FromUtf8Range arg_1 arg_2
    //
    // arg_3 :
    //   { a : Bool   -- isOk
    //   , b : String -- result_str
    //   , c : Nat    -- problem_byte_index
    //   , d : I8     -- problem_code
    //   }
    //
    // if arg_3.a then
    //  Ok arg_3.str
    // else
    //  Err (BadUtf8 { byteIndex: arg_3.byteIndex, problem : arg_3.problem })

    let def = crate::def::Def {
        loc_pattern: no_region(Pattern::Identifier(Symbol::ARG_3)),
        loc_expr: no_region(RunLowLevel {
            op: LowLevel::StrFromUtf8Range,
            args: vec![
                (bytes_var, Var(Symbol::ARG_1)),
                (arg_record_var, Var(Symbol::ARG_2)),
            ],
            ret_var: ll_record_var,
        }),
        expr_var: ll_record_var,
        pattern_vars: SendMap::default(),
        annotation: None,
    };

    let cont = If {
        branch_var: ret_var,
        cond_var: bool_var,
        branches: vec![(
            // if-condition
            no_region(
                // arg_2.c -> Bool
                Access {
                    record_var: ll_record_var,
                    ext_var: var_store.fresh(),
                    field: "c_isOk".into(),
                    field_var: var_store.fresh(),
                    loc_expr: Box::new(no_region(Var(Symbol::ARG_3))),
                },
            ),
            // all is good
            no_region(tag(
                "Ok",
                // arg_2.a -> Str
                vec![Access {
                    record_var: ll_record_var,
                    ext_var: var_store.fresh(),
                    field: "b_str".into(),
                    field_var: var_store.fresh(),
                    loc_expr: Box::new(no_region(Var(Symbol::ARG_3))),
                }],
                var_store,
            )),
        )],
        final_else: Box::new(
            // bad!!
            no_region(tag(
                "Err",
                vec![tag(
                    "BadUtf8",
                    vec![
                        Access {
                            record_var: ll_record_var,
                            ext_var: var_store.fresh(),
                            field: "d_problem".into(),
                            field_var: var_store.fresh(),
                            loc_expr: Box::new(no_region(Var(Symbol::ARG_3))),
                        },
                        Access {
                            record_var: ll_record_var,
                            ext_var: var_store.fresh(),
                            field: "a_byteIndex".into(),
                            field_var: var_store.fresh(),
                            loc_expr: Box::new(no_region(Var(Symbol::ARG_3))),
                        },
                    ],
                    var_store,
                )],
                var_store,
            )),
        ),
    };

    let roc_result = LetNonRec(Box::new(def), Box::new(no_region(cont)), ret_var);

    // Only do the business with the let if we're in bounds!

    let bounds_var = var_store.fresh();
    let bounds_bool = var_store.fresh();
    let add_var = var_store.fresh();

    let body = If {
        cond_var: bounds_bool,
        branch_var: ret_var,
        branches: vec![(
            no_region(RunLowLevel {
                op: LowLevel::NumLte,
                args: vec![
                    (
                        bounds_var,
                        RunLowLevel {
                            op: LowLevel::NumAdd,
                            args: vec![
                                (
                                    add_var,
                                    Access {
                                        record_var: arg_record_var,
                                        ext_var: var_store.fresh(),
                                        field: "start".into(),
                                        field_var: var_store.fresh(),
                                        loc_expr: Box::new(no_region(Var(Symbol::ARG_2))),
                                    },
                                ),
                                (
                                    add_var,
                                    Access {
                                        record_var: arg_record_var,
                                        ext_var: var_store.fresh(),
                                        field: "count".into(),
                                        field_var: var_store.fresh(),
                                        loc_expr: Box::new(no_region(Var(Symbol::ARG_2))),
                                    },
                                ),
                            ],
                            ret_var: add_var,
                        },
                    ),
                    (
                        bounds_var,
                        RunLowLevel {
                            op: LowLevel::ListLen,
                            args: vec![(bytes_var, Var(Symbol::ARG_1))],
                            ret_var: bounds_var,
                        },
                    ),
                ],
                ret_var: bounds_bool,
            }),
            no_region(roc_result),
        )],
        final_else: Box::new(
            // else-branch
            no_region(
                // Err
                tag(
                    "Err",
                    vec![tag("OutOfBounds", Vec::new(), var_store)],
                    var_store,
                ),
            ),
        ),
    };

    defn(
        symbol,
        vec![(bytes_var, Symbol::ARG_1), (arg_record_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
}

/// Str.toUtf8 : Str -> List U8
fn str_to_utf8(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::StrToUtf8, var_store)
}

/// List.concat : List elem, List elem -> List elem
fn list_concat(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListConcat,
        args: vec![
            (list_var, Var(Symbol::ARG_1)),
            (list_var, Var(Symbol::ARG_2)),
        ],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1), (list_var, Symbol::ARG_2)],
        var_store,
        body,
        list_var,
    )
}

/// List.repeat : elem, Nat -> List elem
fn list_repeat(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let elem_var = var_store.fresh();
    let len_var = var_store.fresh();
    let list_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListRepeat,
        args: vec![
            (elem_var, Var(Symbol::ARG_1)),
            (len_var, Var(Symbol::ARG_2)),
        ],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(elem_var, Symbol::ARG_1), (len_var, Symbol::ARG_2)],
        var_store,
        body,
        list_var,
    )
}

/// List.single : elem -> List elem
fn list_single(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let elem_var = var_store.fresh();
    let list_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListSingle,
        args: vec![(elem_var, Var(Symbol::ARG_1))],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(elem_var, Symbol::ARG_1)],
        var_store,
        body,
        list_var,
    )
}

/// List.len : List * -> Int
fn list_len(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let len_var = var_store.fresh();
    let list_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListLen,
        args: vec![(list_var, Var(Symbol::ARG_1))],
        ret_var: len_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1)],
        var_store,
        body,
        len_var,
    )
}

/// List.get : List elem, Int -> Result elem [ OutOfBounds ]*
///
/// List.get :
///     Attr (* | u) (List (Attr u a)),
///     Attr * Int
///     -> Attr * (Result (Attr u a) (Attr * [ OutOfBounds ]*))
fn list_get(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_list = Symbol::ARG_1;
    let arg_index = Symbol::ARG_2;
    let bool_var = var_store.fresh();
    let len_var = var_store.fresh();
    let list_var = var_store.fresh();
    let elem_var = var_store.fresh();
    let ret_var = var_store.fresh();

    // Perform a bounds check. If it passes, run LowLevel::ListGetUnsafe
    let body = If {
        cond_var: bool_var,
        branch_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // index < List.len list
                RunLowLevel {
                    op: LowLevel::NumLt,
                    args: vec![
                        (len_var, Var(arg_index)),
                        (
                            len_var,
                            RunLowLevel {
                                op: LowLevel::ListLen,
                                args: vec![(list_var, Var(arg_list))],
                                ret_var: len_var,
                            },
                        ),
                    ],
                    ret_var: bool_var,
                },
            ),
            // then-branch
            no_region(
                // Ok
                tag(
                    "Ok",
                    vec![
                        // List#getUnsafe list index
                        RunLowLevel {
                            op: LowLevel::ListGetUnsafe,
                            args: vec![(list_var, Var(arg_list)), (len_var, Var(arg_index))],
                            ret_var: elem_var,
                        },
                    ],
                    var_store,
                ),
            ),
        )],
        final_else: Box::new(
            // else-branch
            no_region(
                // Err
                tag(
                    "Err",
                    vec![tag("OutOfBounds", Vec::new(), var_store)],
                    var_store,
                ),
            ),
        ),
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1), (len_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
}

/// List.set : List elem, Nat, elem -> List elem
///
/// List.set :
///     Attr (w | u | v) (List (Attr u a)),
///     Attr * Int,
///     Attr (u | v) a
///     -> Attr * (List (Attr u  a))
fn list_set(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_list = Symbol::ARG_1;
    let arg_index = Symbol::ARG_2;
    let arg_elem = Symbol::ARG_3;
    let bool_var = var_store.fresh();
    let len_var = var_store.fresh();
    let elem_var = var_store.fresh();
    let list_arg_var = var_store.fresh(); // Uniqueness type Attr differs between
    let list_ret_var = var_store.fresh(); // the arg list and the returned list

    // Perform a bounds check. If it passes, run LowLevel::ListSet.
    // Otherwise, return the list unmodified.
    let body = If {
        cond_var: bool_var,
        branch_var: list_ret_var,
        branches: vec![(
            // if-condition
            no_region(
                // index < List.len list
                RunLowLevel {
                    op: LowLevel::NumLt,
                    args: vec![
                        (len_var, Var(arg_index)),
                        (
                            len_var,
                            RunLowLevel {
                                op: LowLevel::ListLen,
                                args: vec![(list_arg_var, Var(arg_list))],
                                ret_var: len_var,
                            },
                        ),
                    ],
                    ret_var: bool_var,
                },
            ),
            // then-branch
            no_region(
                // List.setUnsafe list index
                RunLowLevel {
                    op: LowLevel::ListSet,
                    args: vec![
                        (list_arg_var, Var(arg_list)),
                        (len_var, Var(arg_index)),
                        (elem_var, Var(arg_elem)),
                    ],
                    ret_var: list_ret_var,
                },
            ),
        )],
        final_else: Box::new(
            // else-branch
            no_region(Var(arg_list)),
        ),
    };

    defn(
        symbol,
        vec![
            (list_arg_var, Symbol::ARG_1),
            (len_var, Symbol::ARG_2),
            (elem_var, Symbol::ARG_3),
        ],
        var_store,
        body,
        list_ret_var,
    )
}

/// List.swap : List elem, Nat, Nat -> List elem
fn list_swap(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let index1_var = var_store.fresh();
    let index2_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListSwap,
        args: vec![
            (list_var, Var(Symbol::ARG_1)),
            (index1_var, Var(Symbol::ARG_2)),
            (index2_var, Var(Symbol::ARG_3)),
        ],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![
            (list_var, Symbol::ARG_1),
            (index1_var, Symbol::ARG_2),
            (index2_var, Symbol::ARG_3),
        ],
        var_store,
        body,
        list_var,
    )
}

/// List.takeFirst : List elem, Nat -> List elem
fn list_take_first(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let len_var = var_store.fresh();
    let zero = int::<i128>(len_var, Variable::NATURAL, 0);

    let body = RunLowLevel {
        op: LowLevel::ListSublist,
        args: vec![
            (list_var, Var(Symbol::ARG_1)),
            (len_var, zero),
            (len_var, Var(Symbol::ARG_2)),
        ],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1), (len_var, Symbol::ARG_2)],
        var_store,
        body,
        list_var,
    )
}

/// List.takeLast : List elem, Nat -> List elem
fn list_take_last(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let len_var = var_store.fresh();

    let zero = int::<i128>(len_var, Variable::NATURAL, 0);
    let bool_var = var_store.fresh();

    let get_list_len = RunLowLevel {
        op: LowLevel::ListLen,
        args: vec![(list_var, Var(Symbol::ARG_1))],
        ret_var: len_var,
    };

    let get_sub = RunLowLevel {
        op: LowLevel::NumSubWrap,
        args: vec![(len_var, get_list_len), (len_var, Var(Symbol::ARG_2))],
        ret_var: len_var,
    };

    let get_start = If {
        cond_var: bool_var,
        branch_var: len_var,
        branches: vec![(
            no_region(RunLowLevel {
                op: LowLevel::NumGt,
                args: vec![(len_var, get_sub.clone()), (len_var, zero.clone())],
                ret_var: bool_var,
            }),
            no_region(get_sub),
        )],
        final_else: Box::new(no_region(zero)),
    };

    let body = RunLowLevel {
        op: LowLevel::ListSublist,
        args: vec![
            (list_var, Var(Symbol::ARG_1)),
            (len_var, get_start),
            (len_var, Var(Symbol::ARG_2)),
        ],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1), (len_var, Symbol::ARG_2)],
        var_store,
        body,
        list_var,
    )
}

/// List.sublist : List elem, { start : Nat, len : Nat } -> List elem
fn list_sublist(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let rec_var = var_store.fresh();

    let sym_list = Symbol::ARG_1;
    let sym_rec = Symbol::ARG_2;

    let start_var = var_store.fresh();
    let len_var = var_store.fresh();

    let get_start = Access {
        record_var: rec_var,
        ext_var: var_store.fresh(),
        field_var: var_store.fresh(),
        loc_expr: Box::new(no_region(Var(sym_rec))),
        field: "start".into(),
    };

    let get_len = Access {
        record_var: rec_var,
        ext_var: var_store.fresh(),
        field_var: var_store.fresh(),
        loc_expr: Box::new(no_region(Var(sym_rec))),
        field: "len".into(),
    };

    let body = RunLowLevel {
        op: LowLevel::ListSublist,
        args: vec![
            (list_var, Var(sym_list)),
            (start_var, get_start),
            (len_var, get_len),
        ],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(list_var, sym_list), (rec_var, sym_rec)],
        var_store,
        body,
        list_var,
    )
}

/// List.intersperse : List elem, elem -> List elem
fn list_intersperse(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let sep_var = var_store.fresh();

    let list_sym = Symbol::ARG_1;
    let sep_sym = Symbol::ARG_2;

    let clos_var = var_store.fresh();
    let clos_acc_var = var_store.fresh();

    let clos_sym = Symbol::LIST_INTERSPERSE_CLOS;
    let clos_acc_sym = Symbol::ARG_3;
    let clos_elem_sym = Symbol::ARG_4;

    let int_var = var_store.fresh();
    let zero = int::<i128>(int_var, Variable::NATURAL, 0);

    // \acc, elem -> acc |> List.append sep |> List.append elem
    let clos = Closure(ClosureData {
        function_type: clos_var,
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
        return_type: clos_acc_var,
        name: clos_sym,
        recursive: Recursive::NotRecursive,
        captured_symbols: vec![(sep_sym, sep_var)],
        arguments: vec![
            (clos_acc_var, no_region(Pattern::Identifier(clos_acc_sym))),
            (sep_var, no_region(Pattern::Identifier(clos_elem_sym))),
        ],
        loc_body: {
            let append_sep = RunLowLevel {
                op: LowLevel::ListAppend,
                args: vec![(clos_acc_var, Var(clos_acc_sym)), (sep_var, Var(sep_sym))],
                ret_var: clos_acc_var,
            };

            Box::new(no_region(RunLowLevel {
                op: LowLevel::ListAppend,
                args: vec![(clos_acc_var, append_sep), (sep_var, Var(clos_elem_sym))],
                ret_var: clos_acc_var,
            }))
        },
    });

    // List.walk [] l (\acc, elem -> acc |> List.append sep |> List.append elem)
    let acc = RunLowLevel {
        op: LowLevel::ListWalk,
        args: vec![
            (list_var, Var(list_sym)),
            (
                clos_acc_var,
                List {
                    elem_var: sep_var,
                    loc_elems: vec![],
                },
            ),
            (clos_var, clos),
        ],
        ret_var: clos_acc_var,
    };

    let body = RunLowLevel {
        op: LowLevel::ListDropAt,
        args: vec![(clos_acc_var, acc), (int_var, zero)],
        ret_var: clos_acc_var,
    };

    defn(
        symbol,
        vec![(list_var, list_sym), (sep_var, sep_sym)],
        var_store,
        body,
        clos_acc_var,
    )
}

/// List.split : List elem, Nat -> { before: List elem, others: List elem }
fn list_split(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let index_var = var_store.fresh();

    let list_sym = Symbol::ARG_1;
    let index_sym = Symbol::ARG_2;

    let clos_sym = Symbol::LIST_SPLIT_CLOS;
    let clos_start_sym = Symbol::ARG_3;
    let clos_len_sym = Symbol::ARG_4;

    let clos_fun_var = var_store.fresh();
    let clos_start_var = var_store.fresh();
    let clos_len_var = var_store.fresh();
    let clos_ret_var = var_store.fresh();

    let ret_var = var_store.fresh();
    let zero = int::<i128>(index_var, Variable::NATURAL, 0);

    let clos = Closure(ClosureData {
        function_type: clos_fun_var,
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
        return_type: clos_ret_var,
        name: clos_sym,
        recursive: Recursive::NotRecursive,
        captured_symbols: vec![(list_sym, clos_ret_var)],
        arguments: vec![
            (
                clos_start_var,
                no_region(Pattern::Identifier(clos_start_sym)),
            ),
            (clos_len_var, no_region(Pattern::Identifier(clos_len_sym))),
        ],
        loc_body: {
            Box::new(no_region(RunLowLevel {
                op: LowLevel::ListSublist,
                args: vec![
                    (clos_ret_var, Var(list_sym)),
                    (clos_start_var, Var(clos_start_sym)),
                    (clos_len_var, Var(clos_len_sym)),
                ],
                ret_var: clos_ret_var,
            }))
        },
    });

    let fun = Box::new((
        clos_fun_var,
        no_region(clos),
        var_store.fresh(),
        clos_ret_var,
    ));

    let get_before = Call(
        fun.clone(),
        vec![
            (index_var, no_region(zero)),
            (index_var, no_region(Var(index_sym))),
        ],
        CalledVia::Space,
    );

    let get_list_len = RunLowLevel {
        op: LowLevel::ListLen,
        args: vec![(list_var, Var(list_sym))],
        ret_var: index_var,
    };

    let get_others_len = RunLowLevel {
        op: LowLevel::NumSubWrap,
        args: vec![(index_var, get_list_len), (index_var, Var(index_sym))],
        ret_var: index_var,
    };

    let get_others = Call(
        fun,
        vec![
            (index_var, no_region(Var(index_sym))),
            (index_var, no_region(get_others_len)),
        ],
        CalledVia::Space,
    );

    let before = Field {
        var: clos_ret_var,
        region: Region::zero(),
        loc_expr: Box::new(no_region(get_before)),
    };
    let others = Field {
        var: clos_ret_var,
        region: Region::zero(),
        loc_expr: Box::new(no_region(get_others)),
    };

    let body = record(
        vec![("before".into(), before), ("others".into(), others)],
        var_store,
    );

    defn(
        symbol,
        vec![(list_var, list_sym), (index_var, index_sym)],
        var_store,
        body,
        ret_var,
    )
}

/// List.drop : List elem, Nat -> List elem
fn list_drop(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let len_var = var_store.fresh();

    let get_list_len = RunLowLevel {
        op: LowLevel::ListLen,
        args: vec![(list_var, Var(Symbol::ARG_1))],
        ret_var: len_var,
    };

    let get_len = RunLowLevel {
        op: LowLevel::NumSubWrap,
        args: vec![(len_var, get_list_len), (len_var, Var(Symbol::ARG_2))],
        ret_var: len_var,
    };

    let body = RunLowLevel {
        op: LowLevel::ListSublist,
        args: vec![
            (list_var, Var(Symbol::ARG_1)),
            (len_var, Var(Symbol::ARG_2)),
            (len_var, get_len),
        ],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1), (len_var, Symbol::ARG_2)],
        var_store,
        body,
        list_var,
    )
}

/// List.dropAt : List elem, Nat -> List elem
fn list_drop_at(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let index_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListDropAt,
        args: vec![
            (list_var, Var(Symbol::ARG_1)),
            (index_var, Var(Symbol::ARG_2)),
        ],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1), (index_var, Symbol::ARG_2)],
        var_store,
        body,
        list_var,
    )
}

/// List.dropFirst : List elem -> Result { first: elem, others : List elem } [ ListWasEmpty ]*
fn list_drop_first(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let index_var = var_store.fresh();
    let num_var = Variable::NAT;
    let num_precision_var = Variable::NATURAL;

    let body = RunLowLevel {
        op: LowLevel::ListDropAt,
        args: vec![
            (list_var, Var(Symbol::ARG_1)),
            (index_var, int::<i128>(num_var, num_precision_var, 0)),
        ],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1)],
        var_store,
        body,
        list_var,
    )
}

/// List.dropIf : List elem, (elem -> Bool) -> List elem
fn list_drop_if(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let sym_list = Symbol::ARG_1;
    let sym_predicate = Symbol::ARG_2;
    let t_list = var_store.fresh();
    let t_predicate = var_store.fresh();
    let t_keep_predicate = var_store.fresh();
    let t_elem = var_store.fresh();

    // Defer to keepIf for implementation
    // List.dropIf l p = List.keepIf l (\e -> Bool.not (p e))

    let keep_predicate = Closure(ClosureData {
        function_type: t_keep_predicate,
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
        return_type: Variable::BOOL,
        name: Symbol::LIST_DROP_IF_PREDICATE,
        recursive: Recursive::NotRecursive,
        captured_symbols: vec![(sym_predicate, t_predicate)],
        arguments: vec![(t_elem, no_region(Pattern::Identifier(Symbol::ARG_3)))],
        loc_body: {
            let should_drop = Call(
                Box::new((
                    t_predicate,
                    no_region(Var(sym_predicate)),
                    var_store.fresh(),
                    Variable::BOOL,
                )),
                vec![(t_elem, no_region(Var(Symbol::ARG_3)))],
                CalledVia::Space,
            );
            Box::new(no_region(RunLowLevel {
                op: LowLevel::Not,
                args: vec![(Variable::BOOL, should_drop)],
                ret_var: Variable::BOOL,
            }))
        },
    });

    let body = RunLowLevel {
        op: LowLevel::ListKeepIf,
        args: vec![(t_list, Var(sym_list)), (t_keep_predicate, keep_predicate)],
        ret_var: t_list,
    };

    defn(
        symbol,
        vec![(t_list, sym_list), (t_predicate, sym_predicate)],
        var_store,
        body,
        t_list,
    )
}

/// List.dropLast: List elem -> List elem
fn list_drop_last(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let index_var = var_store.fresh();
    let arg_var = var_store.fresh();
    let len_var = Variable::NAT;
    let num_var = len_var;
    let num_precision_var = Variable::NATURAL;

    let body = RunLowLevel {
        op: LowLevel::ListDropAt,
        args: vec![
            (list_var, Var(Symbol::ARG_1)),
            (
                index_var,
                // Num.sub (List.len list) 1
                RunLowLevel {
                    op: LowLevel::NumSubWrap,
                    args: vec![
                        (
                            arg_var,
                            // List.len list
                            RunLowLevel {
                                op: LowLevel::ListLen,
                                args: vec![(list_var, Var(Symbol::ARG_1))],
                                ret_var: len_var,
                            },
                        ),
                        (arg_var, int::<i128>(num_var, num_precision_var, 1)),
                    ],
                    ret_var: len_var,
                },
            ),
        ],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1)],
        var_store,
        body,
        list_var,
    )
}
/// List.append : List elem, elem -> List elem
fn list_append(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let elem_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListAppend,
        args: vec![
            (list_var, Var(Symbol::ARG_1)),
            (elem_var, Var(Symbol::ARG_2)),
        ],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1), (elem_var, Symbol::ARG_2)],
        var_store,
        body,
        list_var,
    )
}

/// List.prepend : List elem, elem -> List elem
fn list_prepend(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let elem_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListPrepend,
        args: vec![
            (list_var, Var(Symbol::ARG_1)),
            (elem_var, Var(Symbol::ARG_2)),
        ],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1), (elem_var, Symbol::ARG_2)],
        var_store,
        body,
        list_var,
    )
}

/// List.join : List (List elem) -> List elem
fn list_join(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let list_of_list_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListJoin,
        args: vec![(list_of_list_var, Var(Symbol::ARG_1))],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(list_of_list_var, Symbol::ARG_1)],
        var_store,
        body,
        list_var,
    )
}

/// List.walk : List elem, state, (state, elem -> state) -> state
fn list_walk(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_3(symbol, LowLevel::ListWalk, var_store)
}

/// List.walkBackwards : List elem, state, (state, elem -> state) -> state
fn list_walk_backwards(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_3(symbol, LowLevel::ListWalkBackwards, var_store)
}

/// List.walkUntil : List elem, state, (state, elem -> [ Continue state, Stop state ]) -> state
fn list_walk_until(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_3(symbol, LowLevel::ListWalkUntil, var_store)
}

/// List.joinMap : List before, (before -> List after) -> List after
fn list_join_map(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let before = var_store.fresh();
    let list_before = var_store.fresh();
    let after = var_store.fresh();
    let list_after = var_store.fresh();
    let before2list_after = var_store.fresh();
    let t_concat_clos = var_store.fresh();
    let mapper_lambda_set = var_store.fresh();

    // \state, elem -> List.concat state (mapper elem)
    let concat_clos = Closure(ClosureData {
        function_type: t_concat_clos,
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
        return_type: list_after,
        name: Symbol::LIST_JOIN_MAP_CONCAT,
        recursive: Recursive::NotRecursive,
        captured_symbols: vec![(Symbol::ARG_2, before2list_after)],
        arguments: vec![
            (list_after, no_region(Pattern::Identifier(Symbol::ARG_3))),
            (before, no_region(Pattern::Identifier(Symbol::ARG_4))),
        ],
        loc_body: {
            let mapper = Box::new((
                before2list_after,
                no_region(Var(Symbol::ARG_2)),
                mapper_lambda_set,
                list_after, // return type
            ));
            // (mapper elem)
            let mapper_elem = Call(
                mapper,
                vec![(before, no_region(Var(Symbol::ARG_4)))],
                CalledVia::Space,
            );
            Box::new(no_region(RunLowLevel {
                op: LowLevel::ListConcat,
                args: vec![(list_after, Var(Symbol::ARG_3)), (list_after, mapper_elem)],
                ret_var: list_after,
            }))
        },
    });

    // List.joinMap = \input_list, mapper ->
    //   List.walk [] input_list (\state, elem -> List.concat state (mapper elem))
    let body = RunLowLevel {
        op: LowLevel::ListWalk,
        args: vec![
            // input_list : List before
            (list_before, Var(Symbol::ARG_1)),
            // [] : List after
            (
                list_after,
                List {
                    elem_var: after,
                    loc_elems: vec![],
                },
            ),
            // \state, elem -> List.concat state (mapper elem)
            (t_concat_clos, concat_clos),
        ],
        ret_var: list_after,
    };

    defn(
        symbol,
        vec![
            (list_before, Symbol::ARG_1),
            (before2list_after, Symbol::ARG_2),
        ],
        var_store,
        body,
        list_after,
    )
}

// min :  List (Num a) -> Result (Num a) [ ListWasEmpty ]*
fn list_min(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let list_var = var_store.fresh();
    let len_var = Variable::NAT;
    let num_var = len_var;
    let num_precision_var = Variable::NATURAL;
    let list_elem_var = var_store.fresh();
    let ret_var = var_store.fresh();
    let closure_var = var_store.fresh();

    // Perform a bounds check. If it passes, delegate to List.getUnsafe.
    let body = If {
        cond_var: bool_var,
        branch_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // List.len list != 0
                RunLowLevel {
                    op: LowLevel::NotEq,
                    args: vec![
                        (len_var, int::<i128>(num_var, num_precision_var, 0)),
                        (
                            len_var,
                            RunLowLevel {
                                op: LowLevel::ListLen,
                                args: vec![(list_var, Var(Symbol::ARG_1))],
                                ret_var: len_var,
                            },
                        ),
                    ],
                    ret_var: bool_var,
                },
            ),
            // list was not empty
            no_region(
                // Ok ( List.walk list (List.getUnsafe list 0) \acc,elem -> if elem < acc then elem else acc )
                tag(
                    "Ok",
                    vec![
                        // List.walk list (List.getUnsafe list 0) \acc,elem -> if elem < acc then elem else acc
                        RunLowLevel {
                            op: LowLevel::ListWalk,
                            args: vec![
                                (list_var, Var(Symbol::ARG_1)),
                                // (List.getUnsafe list 0)
                                (
                                    list_elem_var,
                                    RunLowLevel {
                                        op: LowLevel::ListGetUnsafe,
                                        args: vec![
                                            (list_var, Var(Symbol::ARG_1)),
                                            (arg_var, int::<i128>(num_var, num_precision_var, 0)),
                                        ],
                                        ret_var: list_elem_var,
                                    },
                                ),
                                // \acc,elem -> if elem < acc then elem else acc
                                (closure_var, list_min_lt(list_elem_var, var_store)),
                            ],
                            ret_var: list_elem_var,
                        },
                    ],
                    var_store,
                ),
            ),
        )],
        final_else: Box::new(
            // list was empty
            no_region(
                // Err ListWasEmpty
                tag(
                    "Err",
                    vec![tag("ListWasEmpty", Vec::new(), var_store)],
                    var_store,
                ),
            ),
        ),
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

// \acc,elem -> if elem < acc then elem else acc
fn list_min_lt(list_elem_var: Variable, var_store: &mut VarStore) -> Expr {
    let bool_var = var_store.fresh();
    // if elem < acc then elem else acc
    let body = If {
        cond_var: bool_var,
        branch_var: list_elem_var,
        branches: vec![(
            // if-condition
            no_region(
                // elem < acc
                RunLowLevel {
                    op: LowLevel::NumLt,
                    args: vec![
                        (list_elem_var, Var(Symbol::ARG_4)),
                        (list_elem_var, Var(Symbol::ARG_3)),
                    ],
                    ret_var: bool_var,
                },
            ),
            // return elem
            no_region(Var(Symbol::ARG_4)),
        )],
        // return acc
        final_else: Box::new(no_region(Var(Symbol::ARG_3))),
    };

    defn_help(
        Symbol::LIST_MIN_LT,
        vec![
            (list_elem_var, Symbol::ARG_3),
            (list_elem_var, Symbol::ARG_4),
        ],
        var_store,
        body,
        list_elem_var,
    )
}

// max :  List (Num a) -> Result (Num a) [ ListWasEmpty ]*
fn list_max(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let list_var = var_store.fresh();
    let len_var = Variable::NAT;
    let num_var = len_var;
    let num_precision_var = Variable::NATURAL;
    let list_elem_var = var_store.fresh();
    let ret_var = var_store.fresh();
    let closure_var = var_store.fresh();

    // Perform a bounds check. If it passes, delegate to List.getUnsafe.
    let body = If {
        cond_var: bool_var,
        branch_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // List.len list != 0
                RunLowLevel {
                    op: LowLevel::NotEq,
                    args: vec![
                        (len_var, int::<i128>(num_var, num_precision_var, 0)),
                        (
                            len_var,
                            RunLowLevel {
                                op: LowLevel::ListLen,
                                args: vec![(list_var, Var(Symbol::ARG_1))],
                                ret_var: len_var,
                            },
                        ),
                    ],
                    ret_var: bool_var,
                },
            ),
            // list was not empty
            no_region(
                // Ok ( List.walk list (List.getUnsafe list 0) \acc,elem -> if elem < acc then elem else acc )
                tag(
                    "Ok",
                    vec![
                        // List.walk list (List.getUnsafe list 0) \acc,elem -> if elem < acc then elem else acc
                        RunLowLevel {
                            op: LowLevel::ListWalk,
                            args: vec![
                                (list_var, Var(Symbol::ARG_1)),
                                // (List.getUnsafe list 0)
                                (
                                    list_elem_var,
                                    RunLowLevel {
                                        op: LowLevel::ListGetUnsafe,
                                        args: vec![
                                            (list_var, Var(Symbol::ARG_1)),
                                            (arg_var, int::<i128>(num_var, num_precision_var, 0)),
                                        ],
                                        ret_var: list_elem_var,
                                    },
                                ),
                                // \acc,elem -> if elem < acc then elem else acc
                                (closure_var, list_max_gt(list_elem_var, var_store)),
                            ],
                            ret_var: list_elem_var,
                        },
                    ],
                    var_store,
                ),
            ),
        )],
        final_else: Box::new(
            // list was empty
            no_region(
                // Err ListWasEmpty
                tag(
                    "Err",
                    vec![tag("ListWasEmpty", Vec::new(), var_store)],
                    var_store,
                ),
            ),
        ),
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

// \acc,elem -> if elem > acc then elem else acc
fn list_max_gt(list_elem_var: Variable, var_store: &mut VarStore) -> Expr {
    let bool_var = var_store.fresh();
    // if elem > acc then elem else acc
    let body = If {
        cond_var: bool_var,
        branch_var: list_elem_var,
        branches: vec![(
            // if-condition
            no_region(
                // elem > acc
                RunLowLevel {
                    op: LowLevel::NumGt,
                    args: vec![
                        (list_elem_var, Var(Symbol::ARG_4)),
                        (list_elem_var, Var(Symbol::ARG_3)),
                    ],
                    ret_var: bool_var,
                },
            ),
            // return elem
            no_region(Var(Symbol::ARG_4)),
        )],
        // return acc
        final_else: Box::new(no_region(Var(Symbol::ARG_3))),
    };

    defn_help(
        Symbol::LIST_MAX_GT,
        vec![
            (list_elem_var, Symbol::ARG_3),
            (list_elem_var, Symbol::ARG_4),
        ],
        var_store,
        body,
        list_elem_var,
    )
}

/// List.sum : List (Num a) -> Num a
fn list_sum(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let num_var = var_store.fresh();
    let ret_var = num_var;
    let list_var = var_store.fresh();
    let closure_var = var_store.fresh();

    let function = (
        var_store.fresh(),
        Loc::at_zero(Expr::Var(Symbol::LIST_WALK)),
        var_store.fresh(),
        ret_var,
    );

    let body = Expr::Call(
        Box::new(function),
        vec![
            (list_var, Loc::at_zero(Var(Symbol::ARG_1))),
            (num_var, Loc::at_zero(num(var_store.fresh(), 0))),
            (closure_var, Loc::at_zero(Var(Symbol::NUM_ADD))),
        ],
        CalledVia::Space,
    );

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

/// List.product : List (Num a) -> Num a
fn list_product(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let num_var = var_store.fresh();
    let list_var = var_store.fresh();
    let closure_var = var_store.fresh();

    let function = (
        var_store.fresh(),
        Loc::at_zero(Expr::Var(Symbol::LIST_WALK)),
        var_store.fresh(),
        num_var,
    );

    let body = Expr::Call(
        Box::new(function),
        vec![
            (list_var, Loc::at_zero(Var(Symbol::ARG_1))),
            (num_var, Loc::at_zero(num(var_store.fresh(), 1))),
            (closure_var, Loc::at_zero(Var(Symbol::NUM_MUL))),
        ],
        CalledVia::Space,
    );

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1)],
        var_store,
        body,
        num_var,
    )
}

/// List.keepIf : List elem, (elem -> Bool) -> List elem
fn list_keep_if(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let func_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListKeepIf,
        args: vec![
            (list_var, Var(Symbol::ARG_1)),
            (func_var, Var(Symbol::ARG_2)),
        ],
        ret_var: list_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1), (func_var, Symbol::ARG_2)],
        var_store,
        body,
        list_var,
    )
}

/// List.contains : List elem, elem -> Bool
fn list_contains(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::ListContains, var_store)
}

/// List.keepOks : List before, (before -> Result after *) -> List after
fn list_keep_oks(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::ListKeepOks, var_store)
}

/// List.keepErrs: List before, (before -> Result * after) -> List after
fn list_keep_errs(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::ListKeepErrs, var_store)
}

/// List.range: Int a, Int a -> List (Int a)
fn list_range(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::ListRange, var_store)
}

/// List.map : List before, (before -> after) -> List after
fn list_map(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::ListMap, var_store)
}

/// List.mapWithIndex : List before, (Nat, before -> after) -> List after
fn list_map_with_index(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::ListMapWithIndex, var_store)
}

/// List.map2 : List a, List b, (a, b -> c) -> List c
fn list_map2(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_3(symbol, LowLevel::ListMap2, var_store)
}

/// List.map3 : List a, List b, List c, (a, b, c -> d) -> List d
fn list_map3(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_4(symbol, LowLevel::ListMap3, var_store)
}

/// List.map4 : List a, List b, List c, List d, (a, b, c, d -> e) -> List e
fn list_map4(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_5(symbol, LowLevel::ListMap4, var_store)
}

/// List.sortWith : List a, (a, a -> Ordering) -> List a
fn list_sort_with(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::ListSortWith, var_store)
}

/// List.sortAsc : List (Num a) -> List (Num a)
fn list_sort_asc(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let closure_var = var_store.fresh();
    let ret_var = list_var;

    let function = (
        var_store.fresh(),
        Loc::at_zero(Expr::Var(Symbol::LIST_SORT_WITH)),
        var_store.fresh(),
        ret_var,
    );

    let body = Expr::Call(
        Box::new(function),
        vec![
            (list_var, Loc::at_zero(Var(Symbol::ARG_1))),
            (closure_var, Loc::at_zero(Var(Symbol::NUM_COMPARE))),
        ],
        CalledVia::Space,
    );

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

/// List.sortDesc : List (Num a) -> List (Num a)
fn list_sort_desc(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let num_var = var_store.fresh();
    let closure_var = var_store.fresh();
    let compare_ret_var = var_store.fresh();
    let ret_var = list_var;

    let closure = Closure(ClosureData {
        function_type: closure_var,
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
        return_type: compare_ret_var,
        name: Symbol::LIST_SORT_DESC_COMPARE,
        recursive: Recursive::NotRecursive,
        captured_symbols: vec![],
        arguments: vec![
            (num_var, no_region(Pattern::Identifier(Symbol::ARG_2))),
            (num_var, no_region(Pattern::Identifier(Symbol::ARG_3))),
        ],
        loc_body: {
            Box::new(no_region(RunLowLevel {
                op: LowLevel::NumCompare,
                args: vec![(num_var, Var(Symbol::ARG_3)), (num_var, Var(Symbol::ARG_2))],
                ret_var: compare_ret_var,
            }))
        },
    });

    let function = (
        var_store.fresh(),
        Loc::at_zero(Expr::Var(Symbol::LIST_SORT_WITH)),
        var_store.fresh(),
        ret_var,
    );

    let body = Expr::Call(
        Box::new(function),
        vec![
            (list_var, Loc::at_zero(Var(Symbol::ARG_1))),
            (closure_var, Loc::at_zero(closure)),
        ],
        CalledVia::Space,
    );

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

/// List.any: List elem, (elem -> Bool) -> Bool
fn list_any(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::ListAny, var_store)
}

/// List.all: List elem, (elem -> Bool) -> Bool
fn list_all(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::ListAll, var_store)
}

/// List.find : List elem, (elem -> Bool) -> Result elem [ NotFound ]*
fn list_find(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list = Symbol::ARG_1;
    let find_predicate = Symbol::ARG_2;

    let find_result = Symbol::LIST_FIND_RESULT;

    let t_list = var_store.fresh();
    let t_pred_fn = var_store.fresh();
    let t_bool = var_store.fresh();
    let t_found = var_store.fresh();
    let t_value = var_store.fresh();
    let t_ret = var_store.fresh();
    let t_find_result = var_store.fresh();
    let t_ext_var1 = var_store.fresh();
    let t_ext_var2 = var_store.fresh();

    // ListFindUnsafe returns { value: elem, found: Bool }.
    // When `found` is true, the value was found. Otherwise `List.find` should return `Err ...`
    let find_result_def = Def {
        annotation: None,
        expr_var: t_find_result,
        loc_expr: no_region(RunLowLevel {
            op: LowLevel::ListFindUnsafe,
            args: vec![(t_list, Var(list)), (t_pred_fn, Var(find_predicate))],
            ret_var: t_find_result,
        }),
        loc_pattern: no_region(Pattern::Identifier(find_result)),
        pattern_vars: Default::default(),
    };

    let get_value = Access {
        record_var: t_find_result,
        ext_var: t_ext_var1,
        field_var: t_value,
        loc_expr: Box::new(no_region(Var(find_result))),
        field: "value".into(),
    };

    let get_found = Access {
        record_var: t_find_result,
        ext_var: t_ext_var2,
        field_var: t_found,
        loc_expr: Box::new(no_region(Var(find_result))),
        field: "found".into(),
    };

    let make_ok = tag("Ok", vec![get_value], var_store);

    let make_err = tag(
        "Err",
        vec![tag("NotFound", Vec::new(), var_store)],
        var_store,
    );

    let inspect = If {
        cond_var: t_bool,
        branch_var: t_ret,
        branches: vec![(
            // if-condition
            no_region(get_found),
            no_region(make_ok),
        )],
        final_else: Box::new(no_region(make_err)),
    };

    let body = LetNonRec(
        Box::new(find_result_def),
        Box::new(no_region(inspect)),
        t_ret,
    );

    defn(
        symbol,
        vec![(t_list, Symbol::ARG_1), (t_pred_fn, Symbol::ARG_2)],
        var_store,
        body,
        t_ret,
    )
}

/// Dict.len : Dict * * -> Nat
fn dict_len(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg1_var = var_store.fresh();
    let ret_var = Variable::NAT;

    let body = RunLowLevel {
        op: LowLevel::DictSize,
        args: vec![(arg1_var, Var(Symbol::ARG_1))],
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

/// Dict.empty : Dict * *
fn dict_empty(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let dict_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::DictEmpty,
        args: vec![],
        ret_var: dict_var,
    };

    Def {
        annotation: None,
        expr_var: dict_var,
        loc_expr: Loc::at_zero(body),
        loc_pattern: Loc::at_zero(Pattern::Identifier(symbol)),
        pattern_vars: SendMap::default(),
    }
}

/// Dict.single : k, v -> Dict k v
fn dict_single(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let key_var = var_store.fresh();
    let value_var = var_store.fresh();
    let dict_var = var_store.fresh();

    let empty = RunLowLevel {
        op: LowLevel::DictEmpty,
        args: vec![],
        ret_var: dict_var,
    };

    let body = RunLowLevel {
        op: LowLevel::DictInsert,
        args: vec![
            (dict_var, empty),
            (key_var, Var(Symbol::ARG_1)),
            (value_var, Var(Symbol::ARG_2)),
        ],
        ret_var: dict_var,
    };

    defn(
        symbol,
        vec![(key_var, Symbol::ARG_1), (value_var, Symbol::ARG_2)],
        var_store,
        body,
        dict_var,
    )
}

/// Dict.insert : Dict k v, k, v -> Dict k v
fn dict_insert(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_3(symbol, LowLevel::DictInsert, var_store)
}

/// Dict.remove : Dict k v, k -> Dict k v
fn dict_remove(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::DictRemove, var_store)
}

/// Dict.contains : Dict k v, k -> Bool
fn dict_contains(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::DictContains, var_store)
}

/// Dict.get : Dict k v, k -> Result v [ KeyNotFound ]*
fn dict_get(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_dict = Symbol::ARG_1;
    let arg_key = Symbol::ARG_2;

    let temp_record = Symbol::DICT_GET_RESULT;

    let bool_var = var_store.fresh();
    let flag_var = var_store.fresh();
    let key_var = var_store.fresh();
    let dict_var = var_store.fresh();
    let value_var = var_store.fresh();
    let ret_var = var_store.fresh();

    let temp_record_var = var_store.fresh();
    let ext_var1 = var_store.fresh();
    let ext_var2 = var_store.fresh();

    // NOTE DictGetUnsafe returns a { flag: Bool, value: v }
    // when the flag is True, the value is found and defined;
    // otherwise it is not and `Dict.get` should return `Err ...`
    let def_body = RunLowLevel {
        op: LowLevel::DictGetUnsafe,
        args: vec![(dict_var, Var(arg_dict)), (key_var, Var(arg_key))],
        ret_var: temp_record_var,
    };

    let def = Def {
        annotation: None,
        expr_var: temp_record_var,
        loc_expr: Loc::at_zero(def_body),
        loc_pattern: Loc::at_zero(Pattern::Identifier(temp_record)),
        pattern_vars: Default::default(),
    };

    let get_value = Access {
        record_var: temp_record_var,
        ext_var: ext_var1,
        field_var: value_var,
        loc_expr: Box::new(no_region(Var(temp_record))),
        field: "value".into(),
    };

    let get_flag = Access {
        record_var: temp_record_var,
        ext_var: ext_var2,
        field_var: flag_var,
        loc_expr: Box::new(no_region(Var(temp_record))),
        field: "zflag".into(),
    };

    let make_ok = tag("Ok", vec![get_value], var_store);

    let make_err = tag(
        "Err",
        vec![tag("KeyNotFound", Vec::new(), var_store)],
        var_store,
    );

    let inspect = If {
        cond_var: bool_var,
        branch_var: ret_var,
        branches: vec![(
            // if-condition
            no_region(get_flag),
            no_region(make_ok),
        )],
        final_else: Box::new(no_region(make_err)),
    };

    let body = LetNonRec(Box::new(def), Box::new(no_region(inspect)), ret_var);

    defn(
        symbol,
        vec![(dict_var, Symbol::ARG_1), (key_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
}

/// Dict.keys : Dict k v -> List k
fn dict_keys(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::DictKeys, var_store)
}

/// Dict.values : Dict k v -> List v
fn dict_values(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::DictValues, var_store)
}

/// Dict.union : Dict k v, Dict k v -> Dict k v
fn dict_union(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::DictUnion, var_store)
}

/// Dict.difference : Dict k v, Dict k v -> Dict k v
fn dict_difference(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::DictDifference, var_store)
}

/// Dict.intersection : Dict k v, Dict k v -> Dict k v
fn dict_intersection(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::DictIntersection, var_store)
}

/// Dict.walk : Dict k v, state, (state, k, v -> state) -> state
fn dict_walk(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_3(symbol, LowLevel::DictWalk, var_store)
}

/// Set.empty : Set *
fn set_empty(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let set_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::DictEmpty,
        args: vec![],
        ret_var: set_var,
    };

    Def {
        annotation: None,
        expr_var: set_var,
        loc_expr: Loc::at_zero(body),
        loc_pattern: Loc::at_zero(Pattern::Identifier(symbol)),
        pattern_vars: SendMap::default(),
    }
}

/// Set.single : k -> Set k
fn set_single(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let key_var = var_store.fresh();
    let set_var = var_store.fresh();
    let value_var = Variable::EMPTY_RECORD;

    let empty = RunLowLevel {
        op: LowLevel::DictEmpty,
        args: vec![],
        ret_var: set_var,
    };

    let body = RunLowLevel {
        op: LowLevel::DictInsert,
        args: vec![
            (set_var, empty),
            (key_var, Var(Symbol::ARG_1)),
            (value_var, EmptyRecord),
        ],
        ret_var: set_var,
    };

    defn(
        symbol,
        vec![(key_var, Symbol::ARG_1)],
        var_store,
        body,
        set_var,
    )
}

/// Set.len : Set * -> Nat
fn set_len(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::DictSize, var_store)
}

/// Dict.union : Dict k v, Dict k v -> Dict k v
fn set_union(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::DictUnion, var_store)
}

/// Dict.difference : Dict k v, Dict k v -> Dict k v
fn set_difference(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::DictDifference, var_store)
}

/// Dict.intersection : Dict k v, Dict k v -> Dict k v
fn set_intersection(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::DictIntersection, var_store)
}

/// Set.toList : Set k -> List k
fn set_to_list(symbol: Symbol, var_store: &mut VarStore) -> Def {
    dict_keys(symbol, var_store)
}

/// Set.fromList : List k -> Set k
fn set_from_list(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::SetFromList, var_store)
}

/// Set.insert : Set k, k -> Set k
fn set_insert(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let dict_var = var_store.fresh();
    let key_var = var_store.fresh();
    let val_var = Variable::EMPTY_RECORD;

    let body = RunLowLevel {
        op: LowLevel::DictInsert,
        args: vec![
            (dict_var, Var(Symbol::ARG_1)),
            (key_var, Var(Symbol::ARG_2)),
            (val_var, EmptyRecord),
        ],
        ret_var: dict_var,
    };

    defn(
        symbol,
        vec![(dict_var, Symbol::ARG_1), (key_var, Symbol::ARG_2)],
        var_store,
        body,
        dict_var,
    )
}

/// Set.remove : Set k, k -> Set k
fn set_remove(symbol: Symbol, var_store: &mut VarStore) -> Def {
    dict_remove(symbol, var_store)
}

/// Set.remove : Set k, k -> Set k
fn set_contains(symbol: Symbol, var_store: &mut VarStore) -> Def {
    dict_contains(symbol, var_store)
}

/// Set.walk : Set k,  (accum, k -> accum), accum -> accum
fn set_walk(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let dict_var = var_store.fresh();
    let func_var = var_store.fresh();
    let key_var = var_store.fresh();
    let accum_var = var_store.fresh();
    let wrapper_var = var_store.fresh();

    let user_function = Box::new((
        func_var,
        no_region(Var(Symbol::ARG_3)),
        var_store.fresh(),
        accum_var,
    ));

    let call_func = Call(
        user_function,
        vec![
            (accum_var, no_region(Var(Symbol::ARG_5))),
            (key_var, no_region(Var(Symbol::ARG_6))),
        ],
        CalledVia::Space,
    );

    let wrapper = Closure(ClosureData {
        function_type: wrapper_var,
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
        return_type: accum_var,
        name: Symbol::SET_WALK_USER_FUNCTION,
        recursive: Recursive::NotRecursive,
        captured_symbols: vec![(Symbol::ARG_3, func_var)],
        arguments: vec![
            (accum_var, no_region(Pattern::Identifier(Symbol::ARG_5))),
            (key_var, no_region(Pattern::Identifier(Symbol::ARG_6))),
            (Variable::EMPTY_RECORD, no_region(Pattern::Underscore)),
        ],
        loc_body: Box::new(no_region(call_func)),
    });

    let body = RunLowLevel {
        op: LowLevel::DictWalk,
        args: vec![
            (dict_var, Var(Symbol::ARG_1)),
            (accum_var, Var(Symbol::ARG_2)),
            (wrapper_var, wrapper),
        ],
        ret_var: accum_var,
    };

    defn(
        symbol,
        vec![
            (dict_var, Symbol::ARG_1),
            (accum_var, Symbol::ARG_2),
            (func_var, Symbol::ARG_3),
        ],
        var_store,
        body,
        accum_var,
    )
}

/// Num.rem : Int a, Int a -> Result (Int a) [ DivByZero ]*
fn num_rem(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let num_var = var_store.fresh();
    let unbound_zero_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let ret_var = var_store.fresh();

    let body = If {
        branch_var: ret_var,
        cond_var: bool_var,
        branches: vec![(
            // if condition
            no_region(
                // Num.isNeq arg2 0
                RunLowLevel {
                    op: LowLevel::NotEq,
                    args: vec![
                        (num_var, Var(Symbol::ARG_2)),
                        (num_var, num(unbound_zero_var, 0)),
                    ],
                    ret_var: bool_var,
                },
            ),
            // arg1 was not zero
            no_region(
                // Ok (Int.#remUnsafe arg1 arg2)
                tag(
                    "Ok",
                    vec![
                        // Num.#remUnsafe arg1 arg2
                        RunLowLevel {
                            op: LowLevel::NumRemUnchecked,
                            args: vec![
                                (num_var, Var(Symbol::ARG_1)),
                                (num_var, Var(Symbol::ARG_2)),
                            ],
                            ret_var: num_var,
                        },
                    ],
                    var_store,
                ),
            ),
        )],
        final_else: Box::new(no_region(tag(
            "Err",
            vec![tag("DivByZero", Vec::new(), var_store)],
            var_store,
        ))),
    };

    defn(
        symbol,
        vec![(num_var, Symbol::ARG_1), (num_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
}

/// Num.isMultipleOf : Int a, Int a -> Bool
fn num_is_multiple_of(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::NumIsMultipleOf, var_store)
}

/// Num.neg : Num a -> Num a
fn num_neg(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let num_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumNeg,
        args: vec![(num_var, Var(Symbol::ARG_1))],
        ret_var: num_var,
    };

    defn(
        symbol,
        vec![(num_var, Symbol::ARG_1)],
        var_store,
        body,
        num_var,
    )
}

/// Num.abs : Num a -> Num a
fn num_abs(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let num_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumAbs,
        args: vec![(num_var, Var(Symbol::ARG_1))],
        ret_var: num_var,
    };

    defn(
        symbol,
        vec![(num_var, Symbol::ARG_1)],
        var_store,
        body,
        num_var,
    )
}

/// Num.div : Float, Float -> Result Float [ DivByZero ]*
fn num_div_float(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let num_var = var_store.fresh();
    let unbound_zero_var = var_store.fresh();
    let precision_var = var_store.fresh();
    let ret_var = var_store.fresh();

    let body = If {
        branch_var: ret_var,
        cond_var: bool_var,
        branches: vec![(
            // if-condition
            no_region(
                // Num.neq denominator 0
                RunLowLevel {
                    op: LowLevel::NotEq,
                    args: vec![
                        (num_var, Var(Symbol::ARG_2)),
                        (num_var, float(unbound_zero_var, precision_var, 0.0)),
                    ],
                    ret_var: bool_var,
                },
            ),
            // denominator was not zero
            no_region(
                // Ok (Float.#divUnchecked numerator denominator)
                tag(
                    "Ok",
                    vec![
                        // Num.#divUnchecked numerator denominator
                        RunLowLevel {
                            op: LowLevel::NumDivUnchecked,
                            args: vec![
                                (num_var, Var(Symbol::ARG_1)),
                                (num_var, Var(Symbol::ARG_2)),
                            ],
                            ret_var: num_var,
                        },
                    ],
                    var_store,
                ),
            ),
        )],
        final_else: Box::new(
            // denominator was zero
            no_region(tag(
                "Err",
                vec![tag("DivByZero", Vec::new(), var_store)],
                var_store,
            )),
        ),
    };

    defn(
        symbol,
        vec![(num_var, Symbol::ARG_1), (num_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
}

/// Num.div : Int a , Int a -> Result (Int a) [ DivByZero ]*
fn num_div_int(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let num_var = var_store.fresh();
    let unbound_zero_var = var_store.fresh();
    let unbound_zero_precision_var = var_store.fresh();
    let ret_var = var_store.fresh();

    let body = If {
        branch_var: ret_var,
        cond_var: bool_var,
        branches: vec![(
            // if-condition
            no_region(
                // Num.neq denominator 0
                RunLowLevel {
                    op: LowLevel::NotEq,
                    args: vec![
                        (num_var, Var(Symbol::ARG_2)),
                        (
                            num_var,
                            int::<i128>(unbound_zero_var, unbound_zero_precision_var, 0),
                        ),
                    ],
                    ret_var: bool_var,
                },
            ),
            // denominator was not zero
            no_region(
                // Ok (Int.#divUnchecked numerator denominator)
                tag(
                    "Ok",
                    vec![
                        // Num.#divUnchecked numerator denominator
                        RunLowLevel {
                            op: LowLevel::NumDivUnchecked,
                            args: vec![
                                (num_var, Var(Symbol::ARG_1)),
                                (num_var, Var(Symbol::ARG_2)),
                            ],
                            ret_var: num_var,
                        },
                    ],
                    var_store,
                ),
            ),
        )],
        final_else: Box::new(
            // denominator was zero
            no_region(tag(
                "Err",
                vec![tag("DivByZero", Vec::new(), var_store)],
                var_store,
            )),
        ),
    };

    defn(
        symbol,
        vec![(num_var, Symbol::ARG_1), (num_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
}

/// Num.divCeil : Int a , Int a -> Result (Int a) [ DivByZero ]*
fn num_div_ceil(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let num_var = var_store.fresh();
    let unbound_zero_var = var_store.fresh();
    let unbound_zero_precision_var = var_store.fresh();
    let ret_var = var_store.fresh();

    let body = If {
        branch_var: ret_var,
        cond_var: bool_var,
        branches: vec![(
            // if-condition
            no_region(
                // Num.neq denominator 0
                RunLowLevel {
                    op: LowLevel::NotEq,
                    args: vec![
                        (num_var, Var(Symbol::ARG_2)),
                        (
                            num_var,
                            int::<i128>(unbound_zero_var, unbound_zero_precision_var, 0),
                        ),
                    ],
                    ret_var: bool_var,
                },
            ),
            // denominator was not zero
            no_region(
                // Ok (Int.#divUnchecked numerator denominator)
                tag(
                    "Ok",
                    vec![
                        // Num.#divUnchecked numerator denominator
                        RunLowLevel {
                            op: LowLevel::NumDivCeilUnchecked,
                            args: vec![
                                (num_var, Var(Symbol::ARG_1)),
                                (num_var, Var(Symbol::ARG_2)),
                            ],
                            ret_var: num_var,
                        },
                    ],
                    var_store,
                ),
            ),
        )],
        final_else: Box::new(
            // denominator was zero
            no_region(tag(
                "Err",
                vec![tag("DivByZero", Vec::new(), var_store)],
                var_store,
            )),
        ),
    };

    defn(
        symbol,
        vec![(num_var, Symbol::ARG_1), (num_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
}

/// List.first : List elem -> Result elem [ ListWasEmpty ]*
///
/// List.first :
///     Attr (* | u) (List (Attr u a)),
///     -> Attr * (Result (Attr u a) (Attr * [ OutOfBounds ]*))
fn list_first(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let list_var = var_store.fresh();
    let len_var = Variable::NAT;
    let zero_var = len_var;
    let zero_precision_var = Variable::NATURAL;
    let list_elem_var = var_store.fresh();
    let ret_var = var_store.fresh();

    // Perform a bounds check. If it passes, delegate to List.getUnsafe.
    let body = If {
        cond_var: bool_var,
        branch_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // List.len list != 0
                RunLowLevel {
                    op: LowLevel::NotEq,
                    args: vec![
                        (len_var, int::<i128>(zero_var, zero_precision_var, 0)),
                        (
                            len_var,
                            RunLowLevel {
                                op: LowLevel::ListLen,
                                args: vec![(list_var, Var(Symbol::ARG_1))],
                                ret_var: len_var,
                            },
                        ),
                    ],
                    ret_var: bool_var,
                },
            ),
            // list was not empty
            no_region(
                // Ok (List.#getUnsafe list 0)
                tag(
                    "Ok",
                    vec![
                        // List.#getUnsafe list 0
                        RunLowLevel {
                            op: LowLevel::ListGetUnsafe,
                            args: vec![
                                (list_var, Var(Symbol::ARG_1)),
                                (len_var, int::<i128>(zero_var, zero_precision_var, 0)),
                            ],
                            ret_var: list_elem_var,
                        },
                    ],
                    var_store,
                ),
            ),
        )],
        final_else: Box::new(
            // list was empty
            no_region(
                // Err ListWasEmpty
                tag(
                    "Err",
                    vec![tag("ListWasEmpty", Vec::new(), var_store)],
                    var_store,
                ),
            ),
        ),
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

/// List.last : List elem -> Result elem [ ListWasEmpty ]*
///
/// List.last :
///     Attr (* | u) (List (Attr u a)),
///     -> Attr * (Result (Attr u a) (Attr * [ OutOfBounds ]*))
fn list_last(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let list_var = var_store.fresh();
    let len_var = Variable::NAT;
    let num_var = len_var;
    let num_precision_var = Variable::NATURAL;
    let list_elem_var = var_store.fresh();
    let ret_var = var_store.fresh();

    // Perform a bounds check. If it passes, delegate to List.getUnsafe.
    let body = If {
        cond_var: bool_var,
        branch_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // List.len list != 0
                RunLowLevel {
                    op: LowLevel::NotEq,
                    args: vec![
                        (len_var, int::<i128>(num_var, num_precision_var, 0)),
                        (
                            len_var,
                            RunLowLevel {
                                op: LowLevel::ListLen,
                                args: vec![(list_var, Var(Symbol::ARG_1))],
                                ret_var: len_var,
                            },
                        ),
                    ],
                    ret_var: bool_var,
                },
            ),
            // list was not empty
            no_region(
                // Ok (List.getUnsafe list (Num.sub (List.len list) 1))
                tag(
                    "Ok",
                    vec![
                        // List.getUnsafe list (Num.sub (List.len list) 1)
                        RunLowLevel {
                            op: LowLevel::ListGetUnsafe,
                            args: vec![
                                (list_var, Var(Symbol::ARG_1)),
                                (
                                    len_var,
                                    // Num.sub (List.len list) 1
                                    RunLowLevel {
                                        op: LowLevel::NumSubWrap,
                                        args: vec![
                                            (
                                                arg_var,
                                                // List.len list
                                                RunLowLevel {
                                                    op: LowLevel::ListLen,
                                                    args: vec![(list_var, Var(Symbol::ARG_1))],
                                                    ret_var: len_var,
                                                },
                                            ),
                                            (arg_var, int::<i128>(num_var, num_precision_var, 1)),
                                        ],
                                        ret_var: len_var,
                                    },
                                ),
                            ],
                            ret_var: list_elem_var,
                        },
                    ],
                    var_store,
                ),
            ),
        )],
        final_else: Box::new(
            // list was empty
            no_region(
                // Err ListWasEmpty
                tag(
                    "Err",
                    vec![tag("ListWasEmpty", Vec::new(), var_store)],
                    var_store,
                ),
            ),
        ),
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

fn result_map(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let ret_var = var_store.fresh();
    let func_var = var_store.fresh();
    let result_var = var_store.fresh();

    let mut branches = vec![];

    {
        let user_function = Box::new((
            func_var,
            no_region(Var(Symbol::ARG_2)),
            var_store.fresh(),
            var_store.fresh(),
        ));

        let call_func = Call(
            user_function,
            vec![(var_store.fresh(), no_region(Var(Symbol::ARG_5)))],
            CalledVia::Space,
        );

        let tag_name = TagName::Global("Ok".into());

        // ok branch
        let ok = Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: tag_name.clone(),
            arguments: vec![(var_store.fresh(), no_region(call_func))],
        };

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(
                var_store.fresh(),
                no_region(Pattern::Identifier(Symbol::ARG_5)),
            )],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(ok),
            guard: None,
        };

        branches.push(branch);
    }

    {
        // err branch
        let tag_name = TagName::Global("Err".into());

        let err = Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: tag_name.clone(),
            arguments: vec![(var_store.fresh(), no_region(Var(Symbol::ARG_4)))],
        };

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(
                var_store.fresh(),
                no_region(Pattern::Identifier(Symbol::ARG_4)),
            )],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(err),
            guard: None,
        };

        branches.push(branch);
    }

    let body = When {
        cond_var: result_var,
        expr_var: ret_var,
        region: Region::zero(),
        loc_cond: Box::new(no_region(Var(Symbol::ARG_1))),
        branches,
    };

    defn(
        symbol,
        vec![(result_var, Symbol::ARG_1), (func_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
}

fn result_map_err(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let ret_var = var_store.fresh();
    let func_var = var_store.fresh();
    let result_var = var_store.fresh();

    let mut branches = vec![];

    {
        let user_function = Box::new((
            func_var,
            no_region(Var(Symbol::ARG_2)),
            var_store.fresh(),
            var_store.fresh(),
        ));

        let call_func = Call(
            user_function,
            vec![(var_store.fresh(), no_region(Var(Symbol::ARG_5)))],
            CalledVia::Space,
        );

        let tag_name = TagName::Global("Err".into());

        // ok branch
        let ok = Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: tag_name.clone(),
            arguments: vec![(var_store.fresh(), no_region(call_func))],
        };

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(
                var_store.fresh(),
                no_region(Pattern::Identifier(Symbol::ARG_5)),
            )],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(ok),
            guard: None,
        };

        branches.push(branch);
    }

    {
        // err branch
        let tag_name = TagName::Global("Ok".into());

        let err = Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: tag_name.clone(),
            arguments: vec![(var_store.fresh(), no_region(Var(Symbol::ARG_4)))],
        };

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(
                var_store.fresh(),
                no_region(Pattern::Identifier(Symbol::ARG_4)),
            )],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(err),
            guard: None,
        };

        branches.push(branch);
    }

    let body = When {
        cond_var: result_var,
        expr_var: ret_var,
        region: Region::zero(),
        loc_cond: Box::new(no_region(Var(Symbol::ARG_1))),
        branches,
    };

    defn(
        symbol,
        vec![(result_var, Symbol::ARG_1), (func_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
}

fn result_with_default(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let ret_var = var_store.fresh();
    let result_var = var_store.fresh();

    let mut branches = vec![];

    {
        // ok branch
        let tag_name = TagName::Global("Ok".into());

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(ret_var, no_region(Pattern::Identifier(Symbol::ARG_3)))],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(Var(Symbol::ARG_3)),
            guard: None,
        };

        branches.push(branch);
    }

    {
        // err branch
        let tag_name = TagName::Global("Err".into());

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(var_store.fresh(), no_region(Pattern::Underscore))],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(Var(Symbol::ARG_2)),
            guard: None,
        };

        branches.push(branch);
    }

    let body = When {
        cond_var: result_var,
        expr_var: ret_var,
        region: Region::zero(),
        loc_cond: Box::new(no_region(Var(Symbol::ARG_1))),
        branches,
    };

    defn(
        symbol,
        vec![(result_var, Symbol::ARG_1), (ret_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
}

fn result_is_err(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let ret_var = var_store.fresh();
    let result_var = var_store.fresh();

    let mut branches = vec![];

    {
        // ok branch
        let tag_name = TagName::Global("Ok".into());

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(var_store.fresh(), no_region(Pattern::Underscore))],
        };

        let false_expr = Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: TagName::Global("False".into()),
            arguments: vec![],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(false_expr),
            guard: None,
        };

        branches.push(branch);
    }

    {
        // err branch
        let tag_name = TagName::Global("Err".into());

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(var_store.fresh(), no_region(Pattern::Underscore))],
        };

        let true_expr = Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: TagName::Global("True".into()),
            arguments: vec![],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(true_expr),
            guard: None,
        };

        branches.push(branch);
    }

    let body = When {
        cond_var: result_var,
        expr_var: ret_var,
        region: Region::zero(),
        loc_cond: Box::new(no_region(Var(Symbol::ARG_1))),
        branches,
    };

    defn(
        symbol,
        vec![(result_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

fn result_is_ok(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let ret_var = var_store.fresh();
    let result_var = var_store.fresh();

    let mut branches = vec![];

    {
        // ok branch
        let tag_name = TagName::Global("Ok".into());

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(var_store.fresh(), no_region(Pattern::Underscore))],
        };

        let true_expr = Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: TagName::Global("True".into()),
            arguments: vec![],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(true_expr),
            guard: None,
        };

        branches.push(branch);
    }

    {
        // err branch
        let tag_name = TagName::Global("Err".into());

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(var_store.fresh(), no_region(Pattern::Underscore))],
        };

        let false_expr = Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: TagName::Global("False".into()),
            arguments: vec![],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(false_expr),
            guard: None,
        };

        branches.push(branch);
    }

    let body = When {
        cond_var: result_var,
        expr_var: ret_var,
        region: Region::zero(),
        loc_cond: Box::new(no_region(Var(Symbol::ARG_1))),
        branches,
    };

    defn(
        symbol,
        vec![(result_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

fn result_after(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let ret_var = var_store.fresh();
    let func_var = var_store.fresh();
    let result_var = var_store.fresh();

    let mut branches = vec![];

    {
        let user_function = Box::new((
            func_var,
            no_region(Var(Symbol::ARG_2)),
            var_store.fresh(),
            var_store.fresh(),
        ));

        let call_func = Call(
            user_function,
            vec![(var_store.fresh(), no_region(Var(Symbol::ARG_5)))],
            CalledVia::Space,
        );

        let tag_name = TagName::Global("Ok".into());

        // ok branch
        let ok = call_func;

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(
                var_store.fresh(),
                no_region(Pattern::Identifier(Symbol::ARG_5)),
            )],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(ok),
            guard: None,
        };

        branches.push(branch);
    }

    {
        // err branch
        let tag_name = TagName::Global("Err".into());

        let err = Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: tag_name.clone(),
            arguments: vec![(var_store.fresh(), no_region(Var(Symbol::ARG_4)))],
        };

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(
                var_store.fresh(),
                no_region(Pattern::Identifier(Symbol::ARG_4)),
            )],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(err),
            guard: None,
        };

        branches.push(branch);
    }

    let body = When {
        cond_var: result_var,
        expr_var: ret_var,
        region: Region::zero(),
        loc_cond: Box::new(no_region(Var(Symbol::ARG_1))),
        branches,
    };

    defn(
        symbol,
        vec![(result_var, Symbol::ARG_1), (func_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
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
        variant_var: var_store.fresh(),
        ext_var: var_store.fresh(),
        name: TagName::Global(name.into()),
        arguments: args
            .into_iter()
            .map(|expr| (var_store.fresh(), no_region(expr)))
            .collect::<Vec<(Variable, Loc<Expr>)>>(),
    }
}

#[inline(always)]
fn record(fields: Vec<(Lowercase, Field)>, var_store: &mut VarStore) -> Expr {
    let mut send_map = SendMap::default();
    for (k, v) in fields {
        send_map.insert(k, v);
    }
    Expr::Record {
        record_var: var_store.fresh(),
        fields: send_map,
    }
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
    }
}

#[inline(always)]
fn num_bytes_to(symbol: Symbol, var_store: &mut VarStore, offset: i64, low_level: LowLevel) -> Def {
    let len_var = var_store.fresh();
    let list_var = var_store.fresh();
    let elem_var = var_store.fresh();

    let ret_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let add_var = var_store.fresh();
    let cast_var = var_store.fresh();

    // Perform a bounds check. If it passes, run LowLevel::low_level
    let body = If {
        cond_var: bool_var,
        branch_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // index + offset < List.len list
                RunLowLevel {
                    op: LowLevel::NumLt,
                    args: vec![
                        (
                            len_var,
                            RunLowLevel {
                                op: LowLevel::NumAdd,
                                args: vec![
                                    (add_var, Var(Symbol::ARG_2)),
                                    (
                                        add_var,
                                        RunLowLevel {
                                            ret_var: cast_var,
                                            args: vec![(cast_var, num(var_store.fresh(), offset))],
                                            op: LowLevel::NumIntCast,
                                        },
                                    ),
                                ],
                                ret_var: add_var,
                            },
                        ),
                        (
                            len_var,
                            RunLowLevel {
                                op: LowLevel::ListLen,
                                args: vec![(list_var, Var(Symbol::ARG_1))],
                                ret_var: len_var,
                            },
                        ),
                    ],
                    ret_var: bool_var,
                },
            ),
            // then-branch
            no_region(
                // Ok
                tag(
                    "Ok",
                    vec![RunLowLevel {
                        op: low_level,
                        args: vec![
                            (list_var, Var(Symbol::ARG_1)),
                            (len_var, Var(Symbol::ARG_2)),
                        ],
                        ret_var: elem_var,
                    }],
                    var_store,
                ),
            ),
        )],
        final_else: Box::new(
            // else-branch
            no_region(
                // Err
                tag(
                    "Err",
                    vec![tag("OutOfBounds", Vec::new(), var_store)],
                    var_store,
                ),
            ),
        ),
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1), (len_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
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
        .map(|(var, symbol)| (var, no_region(Identifier(symbol))))
        .collect();

    Closure(ClosureData {
        function_type: var_store.fresh(),
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
        return_type: ret_var,
        name: fn_name,
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments: closure_args,
        loc_body: Box::new(no_region(body)),
    })
}

#[inline(always)]
fn int_min_or_max<I128>(symbol: Symbol, var_store: &mut VarStore, i: I128) -> Def
where
    I128: Into<i128>,
{
    let int_var = var_store.fresh();
    let int_precision_var = var_store.fresh();
    let body = int::<I128>(int_var, int_precision_var, i);

    let std = roc_builtins::std::types();
    let solved = std.get(&symbol).unwrap();
    let mut free_vars = roc_types::solved_types::FreeVars::default();
    let signature = roc_types::solved_types::to_type(&solved.0, &mut free_vars, var_store);

    let annotation = crate::def::Annotation {
        signature,
        introduced_variables: Default::default(),
        region: Region::zero(),
        aliases: Default::default(),
    };

    Def {
        annotation: Some(annotation),
        expr_var: int_var,
        loc_expr: Loc::at_zero(body),
        loc_pattern: Loc::at_zero(Pattern::Identifier(symbol)),
        pattern_vars: SendMap::default(),
    }
}

#[inline(always)]
fn int<I128>(num_var: Variable, precision_var: Variable, i: I128) -> Expr
where
    I128: Into<i128>,
{
    let ii = i.into();
    Int(num_var, precision_var, ii.to_string().into_boxed_str(), ii)
}

#[inline(always)]
fn float(num_var: Variable, precision_var: Variable, f: f64) -> Expr {
    Float(num_var, precision_var, f.to_string().into_boxed_str(), f)
}

#[inline(always)]
fn num(num_var: Variable, i: i64) -> Expr {
    Num(num_var, i.to_string().into_boxed_str(), i)
}
