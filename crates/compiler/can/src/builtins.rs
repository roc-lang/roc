use crate::def::Def;
use crate::expr::{self, AnnotatedMark, ClosureData, Expr::*, IntValue};
use crate::expr::{Expr, Field, Recursive};
use crate::num::{FloatBound, IntBound, IntWidth, NumBound};
use crate::pattern::Pattern;
use roc_collections::all::SendMap;
use roc_module::called_via::CalledVia;
use roc_module::ident::{Lowercase, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::{ExhaustiveMark, RedundantMark, VarStore, Variable};

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
/// List.get : List elem, Nat -> Result elem [OutOfBounds]*
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
        Symbol::LIST_SET => &[Symbol::LIST_REPLACE],
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
        STR_TO_SCALARS => str_to_scalars,
        STR_GET_UNSAFE => str_get_unsafe,
        STR_SPLIT => str_split,
        STR_IS_EMPTY => str_is_empty,
        STR_STARTS_WITH => str_starts_with,
        STR_STARTS_WITH_SCALAR => str_starts_with_scalar,
        STR_ENDS_WITH => str_ends_with,
        STR_COUNT_GRAPHEMES => str_count_graphemes,
        STR_COUNT_UTF8_BYTES => str_count_bytes,
        STR_SUBSTRING_UNSAFE => str_substring_unsafe,
        STR_RESERVE => str_reserve,
        STR_APPEND_SCALAR_UNSAFE => str_append_scalar_unsafe,
        STR_GET_SCALAR_UNSAFE => str_get_scalar_unsafe,
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
        LIST_WITH_CAPACITY => list_with_capacity,
        LIST_GET_UNSAFE => list_get_unsafe,
        LIST_REPLACE_UNSAFE => list_replace_unsafe,
        LIST_APPEND => list_append,
        LIST_IS_EMPTY => list_is_empty,
        LIST_CONCAT => list_concat,
        LIST_PREPEND => list_prepend,
        LIST_MAP => list_map,
        LIST_MAP2 => list_map2,
        LIST_MAP3 => list_map3,
        LIST_MAP4 => list_map4,
        LIST_SUBLIST => list_sublist,
        LIST_SPLIT => list_split,
        LIST_DROP => list_drop,
        LIST_DROP_AT => list_drop_at,
        LIST_SWAP => list_swap,
        LIST_SORT_WITH => list_sort_with,
        LIST_IS_UNIQUE => list_is_unique,
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
        SET_TO_DICT=> set_to_dict,
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
        NUM_MUL_SATURATED => num_mul_saturated,
        NUM_MUL_CHECKED => num_mul_checked,
        NUM_GT => num_gt,
        NUM_GTE => num_gte,
        NUM_LT => num_lt,
        NUM_LTE => num_lte,
        NUM_COMPARE => num_compare,
        NUM_SIN => num_sin,
        NUM_COS => num_cos,
        NUM_TAN => num_tan,
        NUM_DIV_FRAC => num_div_frac,
        NUM_DIV_FRAC_CHECKED => num_div_frac_checked,
        NUM_DIV_TRUNC => num_div_trunc,
        NUM_DIV_TRUNC_CHECKED => num_div_trunc_checked,
        NUM_DIV_CEIL => num_div_ceil,
        NUM_DIV_CEIL_CHECKED => num_div_ceil_checked,
        NUM_ABS => num_abs,
        NUM_NEG => num_neg,
        NUM_REM => num_rem,
        NUM_REM_CHECKED => num_rem_checked,
        NUM_IS_MULTIPLE_OF => num_is_multiple_of,
        NUM_SQRT => num_sqrt,
        NUM_SQRT_CHECKED => num_sqrt_checked,
        NUM_LOG => num_log,
        NUM_LOG_CHECKED => num_log_checked,
        NUM_ROUND => num_round,
        NUM_IS_ODD => num_is_odd,
        NUM_IS_EVEN => num_is_even,
        NUM_IS_ZERO => num_is_zero,
        NUM_IS_POSITIVE => num_is_positive,
        NUM_IS_NEGATIVE => num_is_negative,
        NUM_TO_FRAC => num_to_frac,
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
        NUM_TO_I8 => num_to_i8,
        NUM_TO_I8_CHECKED => num_to_i8_checked,
        NUM_TO_I16 => num_to_i16,
        NUM_TO_I16_CHECKED => num_to_i16_checked,
        NUM_TO_I32 => num_to_i32,
        NUM_TO_I32_CHECKED => num_to_i32_checked,
        NUM_TO_I64 => num_to_i64,
        NUM_TO_I64_CHECKED => num_to_i64_checked,
        NUM_TO_I128 => num_to_i128,
        NUM_TO_I128_CHECKED => num_to_i128_checked,
        NUM_TO_U8 => num_to_u8,
        NUM_TO_U8_CHECKED => num_to_u8_checked,
        NUM_TO_U16 => num_to_u16,
        NUM_TO_U16_CHECKED => num_to_u16_checked,
        NUM_TO_U32 => num_to_u32,
        NUM_TO_U32_CHECKED => num_to_u32_checked,
        NUM_TO_U64 => num_to_u64,
        NUM_TO_U64_CHECKED => num_to_u64_checked,
        NUM_TO_U128 => num_to_u128,
        NUM_TO_U128_CHECKED => num_to_u128_checked,
        NUM_TO_NAT => num_to_nat,
        NUM_TO_NAT_CHECKED => num_to_nat_checked,
        NUM_TO_F32 => num_to_f32,
        NUM_TO_F32_CHECKED => num_to_f32_checked,
        NUM_TO_F64 => num_to_f64,
        NUM_TO_F64_CHECKED => num_to_f64_checked,
        NUM_TO_STR => num_to_str,
        RESULT_MAP => result_map,
        RESULT_MAP_ERR => result_map_err,
        RESULT_AFTER => result_after,
        RESULT_WITH_DEFAULT => result_with_default,
        RESULT_IS_OK => result_is_ok,
        RESULT_IS_ERR => result_is_err,
        BOX_BOX_FUNCTION => box_box,
        BOX_UNBOX => box_unbox,
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

// Num.toI8 : Int * -> I8
fn num_to_i8(symbol: Symbol, var_store: &mut VarStore) -> Def {
    // Defer to IntCast
    lowlevel_1(symbol, LowLevel::NumIntCast, var_store)
}

// Num.toI16 : Int * -> I16
fn num_to_i16(symbol: Symbol, var_store: &mut VarStore) -> Def {
    // Defer to IntCast
    lowlevel_1(symbol, LowLevel::NumIntCast, var_store)
}

// Num.toI32 : Int * -> I32
fn num_to_i32(symbol: Symbol, var_store: &mut VarStore) -> Def {
    // Defer to IntCast
    lowlevel_1(symbol, LowLevel::NumIntCast, var_store)
}

// Num.toI64 : Int * -> I64
fn num_to_i64(symbol: Symbol, var_store: &mut VarStore) -> Def {
    // Defer to IntCast
    lowlevel_1(symbol, LowLevel::NumIntCast, var_store)
}

// Num.toI128 : Int * -> I128
fn num_to_i128(symbol: Symbol, var_store: &mut VarStore) -> Def {
    // Defer to IntCast
    lowlevel_1(symbol, LowLevel::NumIntCast, var_store)
}

// Num.toU8 : Int * -> U8
fn num_to_u8(symbol: Symbol, var_store: &mut VarStore) -> Def {
    // Defer to IntCast
    lowlevel_1(symbol, LowLevel::NumIntCast, var_store)
}

// Num.toU16 : Int * -> U16
fn num_to_u16(symbol: Symbol, var_store: &mut VarStore) -> Def {
    // Defer to IntCast
    lowlevel_1(symbol, LowLevel::NumIntCast, var_store)
}

// Num.toU32 : Int * -> U32
fn num_to_u32(symbol: Symbol, var_store: &mut VarStore) -> Def {
    // Defer to IntCast
    lowlevel_1(symbol, LowLevel::NumIntCast, var_store)
}

// Num.toU64 : Int * -> U64
fn num_to_u64(symbol: Symbol, var_store: &mut VarStore) -> Def {
    // Defer to IntCast
    lowlevel_1(symbol, LowLevel::NumIntCast, var_store)
}

// Num.toU128 : Int * -> U128
fn num_to_u128(symbol: Symbol, var_store: &mut VarStore) -> Def {
    // Defer to IntCast
    lowlevel_1(symbol, LowLevel::NumIntCast, var_store)
}

// Num.toNat : Int * -> Nat
fn num_to_nat(symbol: Symbol, var_store: &mut VarStore) -> Def {
    // Defer to IntCast
    lowlevel_1(symbol, LowLevel::NumIntCast, var_store)
}

// Num.toF32 : Num * -> F32
fn num_to_f32(symbol: Symbol, var_store: &mut VarStore) -> Def {
    // Defer to NumToFloatCast
    lowlevel_1(symbol, LowLevel::NumToFloatCast, var_store)
}

// Num.toF64 : Num * -> F64
fn num_to_f64(symbol: Symbol, var_store: &mut VarStore) -> Def {
    // Defer to NumToFloatCast
    lowlevel_1(symbol, LowLevel::NumToFloatCast, var_store)
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
                Access {
                    record_var,
                    ext_var: var_store.fresh(),
                    field: "b".into(),
                    field_var: var_store.fresh(),
                    loc_expr: Box::new(no_region(Var(Symbol::ARG_2))),
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
                        Access {
                            record_var,
                            ext_var: var_store.fresh(),
                            field: "a".into(),
                            field_var: num_var_2,
                            loc_expr: Box::new(no_region(Var(Symbol::ARG_2))),
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
            args: vec![(num_var_1, Var(Symbol::ARG_1))],
            ret_var: record_var,
        }),
        expr_var: record_var,
        pattern_vars: SendMap::default(),
        annotation: None,
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

macro_rules! num_to_checked {
    ($($fn:ident)*) => {$(
        // Num.toXXXChecked : Int * -> Result XXX [OutOfBounds]*
        fn $fn(symbol: Symbol, var_store: &mut VarStore) -> Def {
            // Use the generic `NumToIntChecked`; we'll figure out exactly what layout(s) we need
            // during code generation after types are resolved.
            to_num_checked(symbol, var_store, LowLevel::NumToIntChecked)
        }
    )*}
}

num_to_checked! {
    num_to_i8_checked
    num_to_i16_checked
    num_to_i32_checked
    num_to_i64_checked
    num_to_i128_checked
    num_to_u8_checked
    num_to_u16_checked
    num_to_u32_checked
    num_to_u64_checked
    num_to_u128_checked
    num_to_nat_checked
    num_to_f32_checked
    num_to_f64_checked
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

fn num_unaryop(symbol: Symbol, var_store: &mut VarStore, op: LowLevel) -> Def {
    let num_var = var_store.fresh();
    let body = RunLowLevel {
        op,
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

    let body = LetNonRec(Box::new(def), Box::new(no_region(cont)));

    defn(
        symbol,
        vec![(num_var_1, Symbol::ARG_1), (num_var_2, Symbol::ARG_2)],
        var_store,
        body,
        ret_var,
    )
}

/// Num.addChecked : Num a, Num a -> Result (Num a) [Overflow]*
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

/// Num.subChecked : Num a, Num a -> Result (Num a) [Overflow]*
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

/// Num.mulSaturated : Num a, Num a -> Num a
fn num_mul_saturated(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumMulSaturated)
}

/// Num.mulChecked : Num a, Num a -> Result (Num a) [Overflow]*
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

/// Num.compare : Num a, Num a -> [LT, EQ, GT]
fn num_compare(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_num_other_binop(symbol, var_store, LowLevel::NumCompare)
}

/// Num.sin : Frac -> Frac
fn num_sin(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let frac_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::NumSin,
        args: vec![(frac_var, Var(Symbol::ARG_1))],
        ret_var: frac_var,
    };

    defn(
        symbol,
        vec![(frac_var, Symbol::ARG_1)],
        var_store,
        body,
        frac_var,
    )
}

/// Num.cos : Frac -> Frac
fn num_cos(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let frac_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::NumCos,
        args: vec![(frac_var, Var(Symbol::ARG_1))],
        ret_var: frac_var,
    };

    defn(
        symbol,
        vec![(frac_var, Symbol::ARG_1)],
        var_store,
        body,
        frac_var,
    )
}

/// Num.tan : Frac -> Frac
fn num_tan(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let frac_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::NumDivUnchecked,
        args: vec![
            (
                frac_var,
                RunLowLevel {
                    op: LowLevel::NumSin,
                    args: vec![(frac_var, Var(Symbol::ARG_1))],
                    ret_var: frac_var,
                },
            ),
            (
                frac_var,
                RunLowLevel {
                    op: LowLevel::NumCos,
                    args: vec![(frac_var, Var(Symbol::ARG_1))],
                    ret_var: frac_var,
                },
            ),
        ],
        ret_var: frac_var,
    };

    defn(
        symbol,
        vec![(frac_var, Symbol::ARG_1)],
        var_store,
        body,
        frac_var,
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
            (arg_var, num(unbound_zero_var, 0, num_no_bound())),
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
            (arg_var, num(unbound_zero_var, 0, num_no_bound())),
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
            (arg_var, num(unbound_zero_var, 0, num_no_bound())),
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
                int::<i128>(var_store.fresh(), var_store.fresh(), 1, int_no_bound()),
            ),
            (
                arg_var,
                RunLowLevel {
                    op: LowLevel::NumRemUnchecked,
                    args: vec![
                        (arg_var, Var(Symbol::ARG_1)),
                        (arg_var, num(unbound_two_var, 2, num_no_bound())),
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
            (arg_var, num(arg_num_var, 0, num_no_bound())),
            (
                arg_var,
                RunLowLevel {
                    op: LowLevel::NumRemUnchecked,
                    args: vec![
                        (arg_var, Var(Symbol::ARG_1)),
                        (arg_var, num(arg_num_var, 2, num_no_bound())),
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

/// Num.toFrac : Num * -> Frac
fn num_to_frac(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let frac_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumToFrac,
        args: vec![(arg_var, Var(Symbol::ARG_1))],
        ret_var: frac_var,
    };

    defn(
        symbol,
        vec![(arg_var, Symbol::ARG_1)],
        var_store,
        body,
        frac_var,
    )
}

/// Num.sqrt : Frac a -> Frac a
fn num_sqrt(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_unaryop(symbol, var_store, LowLevel::NumSqrtUnchecked)
}

/// Num.sqrtChecked : Frac a -> Result (Frac a) [SqrtOfNegative]*
fn num_sqrt_checked(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let frac_var = var_store.fresh();
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
                    (frac_var, Var(Symbol::ARG_1)),
                    (
                        frac_var,
                        frac(unbound_zero_var, precision_var, 0.0, float_no_bound()),
                    ),
                ],
                ret_var: bool_var,
            }),
            no_region(tag(
                "Ok",
                vec![RunLowLevel {
                    op: LowLevel::NumSqrtUnchecked,
                    args: vec![(frac_var, Var(Symbol::ARG_1))],
                    ret_var: frac_var,
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
        vec![(frac_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

/// Num.log : Frac a -> Frac a
fn num_log(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_unaryop(symbol, var_store, LowLevel::NumLogUnchecked)
}

/// Num.logChecked : Frac a -> Result (Frac a) [LogNeedsPositive]*
fn num_log_checked(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let frac_var = var_store.fresh();
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
                    (frac_var, Var(Symbol::ARG_1)),
                    (
                        frac_var,
                        frac(unbound_zero_var, precision_var, 0.0, float_no_bound()),
                    ),
                ],
                ret_var: bool_var,
            }),
            no_region(tag(
                "Ok",
                vec![RunLowLevel {
                    op: LowLevel::NumLogUnchecked,
                    args: vec![(frac_var, Var(Symbol::ARG_1))],
                    ret_var: frac_var,
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
        vec![(frac_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}

/// Num.round : Frac -> Int
fn num_round(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let frac_var = var_store.fresh();
    let int_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumRound,
        args: vec![(frac_var, Var(Symbol::ARG_1))],
        ret_var: int_var,
    };

    defn(
        symbol,
        vec![(frac_var, Symbol::ARG_1)],
        var_store,
        body,
        int_var,
    )
}

/// Num.pow : Frac, Frac -> Frac
fn num_pow(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let frac_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumPow,
        args: vec![
            (frac_var, Var(Symbol::ARG_1)),
            (frac_var, Var(Symbol::ARG_2)),
        ],
        ret_var: frac_var,
    };

    defn(
        symbol,
        vec![(frac_var, Symbol::ARG_1), (frac_var, Symbol::ARG_2)],
        var_store,
        body,
        frac_var,
    )
}

/// Num.ceiling : Frac -> Int
fn num_ceiling(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let frac_var = var_store.fresh();
    let int_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumCeiling,
        args: vec![(frac_var, Var(Symbol::ARG_1))],
        ret_var: int_var,
    };

    defn(
        symbol,
        vec![(frac_var, Symbol::ARG_1)],
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

/// Num.floor : Frac -> Int
fn num_floor(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let frac_var = var_store.fresh();
    let int_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumFloor,
        args: vec![(frac_var, Var(Symbol::ARG_1))],
        ret_var: int_var,
    };

    defn(
        symbol,
        vec![(frac_var, Symbol::ARG_1)],
        var_store,
        body,
        int_var,
    )
}

/// Num.atan : Frac -> Frac
fn num_atan(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_frac_var = var_store.fresh();
    let ret_frac_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumAtan,
        args: vec![(arg_frac_var, Var(Symbol::ARG_1))],
        ret_var: ret_frac_var,
    };

    defn(
        symbol,
        vec![(arg_frac_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_frac_var,
    )
}

/// Num.acos : Frac -> Frac
fn num_acos(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_frac_var = var_store.fresh();
    let ret_frac_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumAcos,
        args: vec![(arg_frac_var, Var(Symbol::ARG_1))],
        ret_var: ret_frac_var,
    };

    defn(
        symbol,
        vec![(arg_frac_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_frac_var,
    )
}

/// Num.asin : Frac -> Frac
fn num_asin(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_frac_var = var_store.fresh();
    let ret_frac_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::NumAsin,
        args: vec![(arg_frac_var, Var(Symbol::ARG_1))],
        ret_var: ret_frac_var,
    };

    defn(
        symbol,
        vec![(arg_frac_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_frac_var,
    )
}

/// Num.bytesToU16 : List U8, Nat -> Result U16 [OutOfBounds]
fn num_bytes_to_u16(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_bytes_to(symbol, var_store, 1, LowLevel::NumBytesToU16)
}

/// Num.bytesToU32 : List U8, Nat -> Result U32 [OutOfBounds]
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

/// List.isEmpty : List * -> Bool
fn list_is_empty(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let len_var = Variable::NAT;
    let unbound_zero_var = Variable::NATURAL;

    let body = RunLowLevel {
        op: LowLevel::Eq,
        args: vec![
            (len_var, num(unbound_zero_var, 0, num_no_bound())),
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

/// Str.toNum : Str -> Result (Num *) [InvalidNumStr]*
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
                        int::<i128>(
                            errorcode_var,
                            Variable::UNSIGNED8,
                            0,
                            IntBound::Exact(IntWidth::U8),
                        ),
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

    let body = LetNonRec(Box::new(def), Box::new(no_region(cont)));

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

/// List.getUnsafe : Str, Nat -> U8
fn str_get_unsafe(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::StrGetUnsafe, var_store)
}

/// Str.toScalars : Str -> List U32
fn str_to_scalars(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::StrToScalars, var_store)
}

/// Str.joinWith : List Str, Str -> Str
fn str_join_with(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::StrJoinWith, var_store)
}

/// Str.isEmpty : Str -> Bool
fn str_is_empty(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::StrIsEmpty, var_store)
}

/// Str.startsWith : Str, Str -> Bool
fn str_starts_with(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::StrStartsWith, var_store)
}

/// Str.startsWithScalar : Str, U32 -> Bool
fn str_starts_with_scalar(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::StrStartsWithScalar, var_store)
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
    lowlevel_1(symbol, LowLevel::StrCountGraphemes, var_store)
}

/// Str.countUtf8Bytes : Str -> Nat
fn str_count_bytes(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::StrCountUtf8Bytes, var_store)
}

/// Str.substringUnsafe : Str, Nat, Nat -> Nat
fn str_substring_unsafe(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_3(symbol, LowLevel::StrSubstringUnsafe, var_store)
}

/// Str.reserve : Str, Nat -> Str
fn str_reserve(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::StrReserve, var_store)
}

/// Str.appendScalarUnsafe : Str, U32 -> Str
fn str_append_scalar_unsafe(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::StrAppendScalar, var_store)
}

/// Str.getScalarUnsafe : Str, Nat -> { scalar : U32, bytesParsed : Nat }
fn str_get_scalar_unsafe(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::StrGetScalarUnsafe, var_store)
}

/// Str.fromUtf8 : List U8 -> Result Str [BadUtf8 { byteIndex : Nat, problem : Utf8Problem  } }]*
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
    //  Err (BadUtf8 arg_2.problem arg_2.byteIndex)

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

    let body = LetNonRec(Box::new(def), Box::new(no_region(cont)));

    defn(
        symbol,
        vec![(bytes_var, Symbol::ARG_1)],
        var_store,
        body,
        ret_var,
    )
}
/// Str.fromUtf8Range : List U8, { start : Nat, count : Nat } -> Result Str [BadUtf8 { byteIndex : Nat, problem : Utf8Problem  } }]*
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
    //  Err (BadUtf8 arg_3.problem arg_3.byteIndex)

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

    let roc_result = LetNonRec(Box::new(def), Box::new(no_region(cont)));

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

/// List.len : List a -> Nat
fn list_len(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::ListLen, var_store)
}

/// List.withCapacity : Nat -> List a
fn list_with_capacity(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::ListWithCapacity, var_store)
}

/// List.getUnsafe : List elem, Int -> elem
fn list_get_unsafe(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::ListGetUnsafe, var_store)
}

/// List.replaceUnsafe : List elem, Nat, elem -> { list: List elem, value: elem }
fn list_replace_unsafe(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_3(symbol, LowLevel::ListReplaceUnsafe, var_store)
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
    let zero = int::<i128>(
        index_var,
        Variable::NATURAL,
        0,
        IntBound::Exact(IntWidth::Nat),
    );

    let clos = Closure(ClosureData {
        function_type: clos_fun_var,
        closure_type: var_store.fresh(),
        return_type: clos_ret_var,
        name: clos_sym,
        recursive: Recursive::NotRecursive,
        captured_symbols: vec![(list_sym, clos_ret_var)],
        arguments: vec![
            (
                clos_start_var,
                AnnotatedMark::new(var_store),
                no_region(Pattern::Identifier(clos_start_sym)),
            ),
            (
                clos_len_var,
                AnnotatedMark::new(var_store),
                no_region(Pattern::Identifier(clos_len_sym)),
            ),
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
    lowlevel_2(symbol, LowLevel::ListDropAt, var_store)
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

/// List.map : List before, (before -> after) -> List after
fn list_map(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::ListMap, var_store)
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

/// List.isUnique : List * -> Bool
fn list_is_unique(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::ListIsUnique, var_store)
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

/// Dict.get : Dict k v, k -> Result v [KeyNotFound]*
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

    let body = LetNonRec(Box::new(def), Box::new(no_region(inspect)));

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

/// Set.toDict : Set k -> Dict k {}
fn set_to_dict(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::SetToDict, var_store)
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
        return_type: accum_var,
        name: Symbol::SET_WALK_USER_FUNCTION,
        recursive: Recursive::NotRecursive,
        captured_symbols: vec![(Symbol::ARG_3, func_var)],
        arguments: vec![
            (
                accum_var,
                AnnotatedMark::new(var_store),
                no_region(Pattern::Identifier(Symbol::ARG_5)),
            ),
            (
                key_var,
                AnnotatedMark::new(var_store),
                no_region(Pattern::Identifier(Symbol::ARG_6)),
            ),
            (
                Variable::EMPTY_RECORD,
                AnnotatedMark::new(var_store),
                no_region(Pattern::Underscore),
            ),
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

/// Num.rem : Int a, Int a -> Int a
fn num_rem(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumRemUnchecked)
}

/// Num.remChecked : Int a, Int a -> Result (Int a) [DivByZero]*
fn num_rem_checked(symbol: Symbol, var_store: &mut VarStore) -> Def {
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
                        (num_var, num(unbound_zero_var, 0, num_no_bound())),
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

/// Num.div : Frac, Frac -> Frac
fn num_div_frac(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumDivUnchecked)
}

/// Num.divChecked : Frac, Frac -> Result Frac [DivByZero]*
fn num_div_frac_checked(symbol: Symbol, var_store: &mut VarStore) -> Def {
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
                        (
                            num_var,
                            frac(unbound_zero_var, precision_var, 0.0, float_no_bound()),
                        ),
                    ],
                    ret_var: bool_var,
                },
            ),
            // denominator was not zero
            no_region(
                // Ok (Frac.#divUnchecked numerator denominator)
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

/// Num.divTrunc : Int a, Int a -> Int a
fn num_div_trunc(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumDivUnchecked)
}

/// Num.divTruncChecked : Int a , Int a -> Result (Int a) [DivByZero]*
fn num_div_trunc_checked(symbol: Symbol, var_store: &mut VarStore) -> Def {
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
                            int::<i128>(
                                unbound_zero_var,
                                unbound_zero_precision_var,
                                0,
                                int_no_bound(),
                            ),
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

/// Num.divCeil : Int a, Int a -> Int a
fn num_div_ceil(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumDivCeilUnchecked)
}

/// Num.divCeilChecked : Int a , Int a -> Result (Int a) [DivByZero]*
fn num_div_ceil_checked(symbol: Symbol, var_store: &mut VarStore) -> Def {
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
                            int::<i128>(
                                unbound_zero_var,
                                unbound_zero_precision_var,
                                0,
                                int_no_bound(),
                            ),
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

        let tag_name = TagName("Ok".into());

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
            redundant: RedundantMark::new(var_store),
        };

        branches.push(branch);
    }

    {
        // err branch
        let tag_name = TagName("Err".into());

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
            redundant: RedundantMark::new(var_store),
        };

        branches.push(branch);
    }

    let body = When {
        cond_var: result_var,
        expr_var: ret_var,
        region: Region::zero(),
        loc_cond: Box::new(no_region(Var(Symbol::ARG_1))),
        branches,
        branches_cond_var: var_store.fresh(),
        exhaustive: ExhaustiveMark::new(var_store),
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

        let tag_name = TagName("Err".into());

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
            redundant: RedundantMark::new(var_store),
        };

        branches.push(branch);
    }

    {
        // err branch
        let tag_name = TagName("Ok".into());

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
            redundant: RedundantMark::new(var_store),
        };

        branches.push(branch);
    }

    let body = When {
        cond_var: result_var,
        expr_var: ret_var,
        region: Region::zero(),
        loc_cond: Box::new(no_region(Var(Symbol::ARG_1))),
        branches,
        branches_cond_var: var_store.fresh(),
        exhaustive: ExhaustiveMark::new(var_store),
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
        let tag_name = TagName("Ok".into());

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
            redundant: RedundantMark::new(var_store),
        };

        branches.push(branch);
    }

    {
        // err branch
        let tag_name = TagName("Err".into());

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
            redundant: RedundantMark::new(var_store),
        };

        branches.push(branch);
    }

    let body = When {
        cond_var: result_var,
        expr_var: ret_var,
        region: Region::zero(),
        loc_cond: Box::new(no_region(Var(Symbol::ARG_1))),
        branches,
        branches_cond_var: var_store.fresh(),
        exhaustive: ExhaustiveMark::new(var_store),
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
        let tag_name = TagName("Ok".into());

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(var_store.fresh(), no_region(Pattern::Underscore))],
        };

        let false_expr = Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: TagName("False".into()),
            arguments: vec![],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(false_expr),
            guard: None,
            redundant: RedundantMark::new(var_store),
        };

        branches.push(branch);
    }

    {
        // err branch
        let tag_name = TagName("Err".into());

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(var_store.fresh(), no_region(Pattern::Underscore))],
        };

        let true_expr = Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: TagName("True".into()),
            arguments: vec![],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(true_expr),
            guard: None,
            redundant: RedundantMark::new(var_store),
        };

        branches.push(branch);
    }

    let body = When {
        cond_var: result_var,
        expr_var: ret_var,
        region: Region::zero(),
        loc_cond: Box::new(no_region(Var(Symbol::ARG_1))),
        branches,
        branches_cond_var: var_store.fresh(),
        exhaustive: ExhaustiveMark::new(var_store),
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
        let tag_name = TagName("Ok".into());

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(var_store.fresh(), no_region(Pattern::Underscore))],
        };

        let true_expr = Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: TagName("True".into()),
            arguments: vec![],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(true_expr),
            guard: None,
            redundant: RedundantMark::new(var_store),
        };

        branches.push(branch);
    }

    {
        // err branch
        let tag_name = TagName("Err".into());

        let pattern = Pattern::AppliedTag {
            whole_var: result_var,
            ext_var: var_store.fresh(),
            tag_name,
            arguments: vec![(var_store.fresh(), no_region(Pattern::Underscore))],
        };

        let false_expr = Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: TagName("False".into()),
            arguments: vec![],
        };

        let branch = expr::WhenBranch {
            patterns: vec![no_region(pattern)],
            value: no_region(false_expr),
            guard: None,
            redundant: RedundantMark::new(var_store),
        };

        branches.push(branch);
    }

    let body = When {
        cond_var: result_var,
        expr_var: ret_var,
        region: Region::zero(),
        loc_cond: Box::new(no_region(Var(Symbol::ARG_1))),
        branches,
        branches_cond_var: var_store.fresh(),
        exhaustive: ExhaustiveMark::new(var_store),
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

        let tag_name = TagName("Ok".into());

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
            redundant: RedundantMark::new(var_store),
        };

        branches.push(branch);
    }

    {
        // err branch
        let tag_name = TagName("Err".into());

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
            redundant: RedundantMark::new(var_store),
        };

        branches.push(branch);
    }

    let body = When {
        cond_var: result_var,
        expr_var: ret_var,
        region: Region::zero(),
        loc_cond: Box::new(no_region(Var(Symbol::ARG_1))),
        branches,
        branches_cond_var: var_store.fresh(),
        exhaustive: ExhaustiveMark::new(var_store),
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
        name: TagName(name.into()),
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
                                            args: vec![(
                                                cast_var,
                                                num(var_store.fresh(), offset, num_no_bound()),
                                            )],
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

/// Box.box : a -> Box a
fn box_box(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::BoxExpr, var_store)
}

/// Box.unbox : Box a -> a
fn box_unbox(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::UnboxExpr, var_store)
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
        name: fn_name,
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments: closure_args,
        loc_body: Box::new(no_region(body)),
    })
}

fn num_no_bound() -> NumBound {
    NumBound::None
}

fn int_no_bound() -> IntBound {
    IntBound::None
}

fn float_no_bound() -> FloatBound {
    FloatBound::None
}

#[inline(always)]
fn int<I128>(num_var: Variable, precision_var: Variable, i: I128, bound: IntBound) -> Expr
where
    I128: Into<i128>,
{
    let ii = i.into();
    Int(
        num_var,
        precision_var,
        ii.to_string().into_boxed_str(),
        IntValue::I128(ii.to_ne_bytes()),
        bound,
    )
}

#[inline(always)]
fn frac(num_var: Variable, precision_var: Variable, f: f64, bound: FloatBound) -> Expr {
    Float(
        num_var,
        precision_var,
        f.to_string().into_boxed_str(),
        f,
        bound,
    )
}

#[inline(always)]
fn num<I: Into<i128>>(num_var: Variable, i: I, bound: NumBound) -> Expr {
    let i = i.into();
    Num(
        num_var,
        i.to_string().into_boxed_str(),
        IntValue::I128(i.to_ne_bytes()),
        bound,
    )
}
