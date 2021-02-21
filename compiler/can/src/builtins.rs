use crate::def::Def;
use crate::expr::Expr::*;
use crate::expr::{Expr, Recursive, WhenBranch};
use crate::pattern::Pattern;
use roc_collections::all::{MutMap, SendMap};
use roc_module::ident::TagName;
use roc_module::low_level::LowLevel;
use roc_module::operator::CalledVia;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::subs::{VarStore, Variable};

macro_rules! defs {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(defs!(@single $rest)),*]));

    ($var_store:expr; $($key:expr => $func:expr,)+) => { defs!($var_store; $($key => $func),+) };
    ($var_store:expr; $($key:expr => $func:expr),*) => {
        {
            let _cap = defs!(@count $($key),*);
            let mut _map = ::std::collections::HashMap::with_capacity_and_hasher(_cap, roc_collections::all::default_hasher());
            $(
                let _ = _map.insert($key, $func($key, $var_store));
            )*
            _map
        }
    };
}

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
        STR_ENDS_WITH => str_ends_with,
        STR_COUNT_GRAPHEMES => str_count_graphemes,
        STR_FROM_INT => str_from_int,
        STR_FROM_UTF8 => str_from_utf8,
        STR_FROM_FLOAT=> str_from_float,
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
        LIST_SUM => list_sum,
        LIST_PREPEND => list_prepend,
        LIST_JOIN => list_join,
        LIST_MAP => list_map,
        LIST_MAP_WITH_INDEX => list_map_with_index,
        LIST_KEEP_IF => list_keep_if,
        LIST_KEEP_OKS => list_keep_oks,
        LIST_KEEP_ERRS=> list_keep_errs,
        LIST_WALK => list_walk,
        LIST_WALK_BACKWARDS => list_walk_backwards,
        DICT_TEST_HASH => dict_hash_test_only,
        DICT_LEN => dict_len,
        DICT_EMPTY => dict_empty,
        DICT_SINGLETON => dict_singleton,
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
        SET_SINGLETON => set_singleton,
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
        NUM_SUB => num_sub,
        NUM_SUB_WRAP => num_sub_wrap,
        NUM_SUB_CHECKED => num_sub_checked,
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
        NUM_ABS => num_abs,
        NUM_NEG => num_neg,
        NUM_REM => num_rem,
        NUM_SQRT => num_sqrt,
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
        NUM_MAX_INT => num_max_int,
        NUM_MIN_INT => num_min_int,
        NUM_BITWISE_AND => num_bitwise_and,
        NUM_BITWISE_XOR => num_bitwise_xor,
        NUM_BITWISE_OR => num_bitwise_or,
        NUM_SHIFT_LEFT=> num_shift_left_by,
        RESULT_MAP => result_map,
        RESULT_MAP_ERR => result_map_err,
        RESULT_WITH_DEFAULT => result_with_default,
    }
}

/// Some builtins cannot be constructed in code gen alone, and need to be defined
/// as separate Roc defs. For example, List.get has this type:
///
/// List.get : List elem, Int -> Result elem [ OutOfBounds ]*
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
pub fn builtin_defs(var_store: &mut VarStore) -> MutMap<Symbol, Def> {
    defs! { var_store;
        Symbol::BOOL_EQ => bool_eq,
        Symbol::BOOL_NEQ => bool_neq,
        Symbol::BOOL_AND => bool_and,
        Symbol::BOOL_OR => bool_or,
        Symbol::BOOL_NOT => bool_not,
        Symbol::STR_CONCAT => str_concat,
        Symbol::STR_JOIN_WITH => str_join_with,
        Symbol::STR_SPLIT => str_split,
        Symbol::STR_IS_EMPTY => str_is_empty,
        Symbol::STR_STARTS_WITH => str_starts_with,
        Symbol::STR_ENDS_WITH => str_ends_with,
        Symbol::STR_COUNT_GRAPHEMES => str_count_graphemes,
        Symbol::STR_FROM_INT => str_from_int,
        Symbol::STR_FROM_UTF8 => str_from_utf8,
        Symbol::STR_FROM_FLOAT=> str_from_float,
        Symbol::LIST_LEN => list_len,
        Symbol::LIST_GET => list_get,
        Symbol::LIST_SET => list_set,
        Symbol::LIST_APPEND => list_append,
        Symbol::LIST_FIRST => list_first,
        Symbol::LIST_LAST => list_last,
        Symbol::LIST_IS_EMPTY => list_is_empty,
        Symbol::LIST_SINGLE => list_single,
        Symbol::LIST_REPEAT => list_repeat,
        Symbol::LIST_REVERSE => list_reverse,
        Symbol::LIST_CONCAT => list_concat,
        Symbol::LIST_CONTAINS => list_contains,
        Symbol::LIST_SUM => list_sum,
        Symbol::LIST_PREPEND => list_prepend,
        Symbol::LIST_JOIN => list_join,
        Symbol::LIST_MAP => list_map,
        Symbol::LIST_MAP_WITH_INDEX => list_map_with_index,
        Symbol::LIST_KEEP_IF => list_keep_if,
        Symbol::LIST_KEEP_OKS => list_keep_oks,
        Symbol::LIST_KEEP_ERRS=> list_keep_errs,
        Symbol::LIST_WALK => list_walk,
        Symbol::LIST_WALK_BACKWARDS => list_walk_backwards,
        Symbol::DICT_TEST_HASH => dict_hash_test_only,
        Symbol::DICT_LEN => dict_len,
        Symbol::DICT_EMPTY => dict_empty,
        Symbol::DICT_SINGLETON => dict_singleton,
        Symbol::DICT_INSERT => dict_insert,
        Symbol::DICT_REMOVE => dict_remove,
        Symbol::DICT_GET => dict_get,
        Symbol::DICT_CONTAINS => dict_contains,
        Symbol::DICT_KEYS => dict_keys,
        Symbol::DICT_VALUES => dict_values,
        Symbol::DICT_UNION=> dict_union,
        Symbol::DICT_INTERSECTION=> dict_intersection,
        Symbol::DICT_DIFFERENCE=> dict_difference,
        Symbol::DICT_WALK=> dict_walk,
        Symbol::SET_EMPTY => set_empty,
        Symbol::SET_LEN => set_len,
        Symbol::SET_SINGLETON => set_singleton,
        Symbol::SET_UNION=> set_union,
        Symbol::SET_INTERSECTION=> set_intersection,
        Symbol::SET_DIFFERENCE=> set_difference,
        Symbol::SET_TO_LIST => set_to_list,
        Symbol::SET_FROM_LIST => set_from_list,
        Symbol::SET_INSERT => set_insert,
        Symbol::SET_REMOVE => set_remove,
        Symbol::SET_CONTAINS => set_contains,
        Symbol::SET_WALK => set_walk,
        Symbol::NUM_ADD => num_add,
        Symbol::NUM_ADD_CHECKED => num_add_checked,
        Symbol::NUM_ADD_WRAP => num_add_wrap,
        Symbol::NUM_SUB => num_sub,
        Symbol::NUM_MUL => num_mul,
        Symbol::NUM_GT => num_gt,
        Symbol::NUM_GTE => num_gte,
        Symbol::NUM_LT => num_lt,
        Symbol::NUM_LTE => num_lte,
        Symbol::NUM_COMPARE => num_compare,
        Symbol::NUM_SIN => num_sin,
        Symbol::NUM_COS => num_cos,
        Symbol::NUM_TAN => num_tan,
        Symbol::NUM_DIV_FLOAT => num_div_float,
        Symbol::NUM_DIV_INT => num_div_int,
        Symbol::NUM_ABS => num_abs,
        Symbol::NUM_NEG => num_neg,
        Symbol::NUM_REM => num_rem,
        Symbol::NUM_SQRT => num_sqrt,
        Symbol::NUM_ROUND => num_round,
        Symbol::NUM_IS_ODD => num_is_odd,
        Symbol::NUM_IS_EVEN => num_is_even,
        Symbol::NUM_IS_ZERO => num_is_zero,
        Symbol::NUM_IS_POSITIVE => num_is_positive,
        Symbol::NUM_IS_NEGATIVE => num_is_negative,
        Symbol::NUM_TO_FLOAT => num_to_float,
        Symbol::NUM_POW => num_pow,
        Symbol::NUM_CEILING => num_ceiling,
        Symbol::NUM_POW_INT => num_pow_int,
        Symbol::NUM_FLOOR => num_floor,
        Symbol::NUM_ATAN => num_atan,
        Symbol::NUM_ACOS => num_acos,
        Symbol::NUM_ASIN => num_asin,
        Symbol::NUM_MAX_INT => num_max_int,
        Symbol::NUM_MIN_INT => num_min_int,
        Symbol::NUM_BITWISE_AND => num_bitwise_and,
        Symbol::NUM_BITWISE_XOR => num_bitwise_xor,
        Symbol::NUM_BITWISE_OR => num_bitwise_or,
        Symbol::NUM_SHIFT_LEFT=> num_shift_left_by,
        Symbol::RESULT_MAP => result_map,
        Symbol::RESULT_MAP_ERR => result_map_err,
        Symbol::RESULT_WITH_DEFAULT => result_with_default,
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

/// Num.maxInt : Int
fn num_max_int(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let int_var = var_store.fresh();
    let int_percision_var = var_store.fresh();
    let body = Int(int_var, int_percision_var, i64::MAX);

    Def {
        annotation: None,
        expr_var: int_var,
        loc_expr: Located::at_zero(body),
        loc_pattern: Located::at_zero(Pattern::Identifier(symbol)),
        pattern_vars: SendMap::default(),
    }
}

/// Num.minInt : Int
fn num_min_int(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let int_var = var_store.fresh();
    let int_percision_var = var_store.fresh();
    let body = Int(int_var, int_percision_var, i64::MIN);

    Def {
        annotation: None,
        expr_var: int_var,
        loc_expr: Located::at_zero(body),
        loc_pattern: Located::at_zero(Pattern::Identifier(symbol)),
        pattern_vars: SendMap::default(),
    }
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

/// Num.addWrap : Int, Int -> Int
fn num_add_wrap(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumAddWrap)
}

/// Num.addChecked : Num a, Num a -> Result (Num a) [ Overflow ]*
fn num_add_checked(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let num_var_1 = var_store.fresh();
    let num_var_2 = var_store.fresh();
    let num_var_3 = var_store.fresh();
    let ret_var = var_store.fresh();
    let record_var = var_store.fresh();

    // let arg_3 = RunLowLevel NumAddChecked arg_1 arg_2
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

    // arg_3 = RunLowLevel NumAddChecked arg_1 arg_2
    let def = crate::def::Def {
        loc_pattern: no_region(Pattern::Identifier(Symbol::ARG_3)),
        loc_expr: no_region(RunLowLevel {
            op: LowLevel::NumAddChecked,
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

/// Num.sub : Num a, Num a -> Num a
fn num_sub(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumSub)
}

/// Num.subWrap : Int, Int -> Int
fn num_sub_wrap(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumSubWrap)
}

/// Num.subChecked : Num a, Num a -> Result (Num a) [ Overflow ]*
fn num_sub_checked(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let num_var_1 = var_store.fresh();
    let num_var_2 = var_store.fresh();
    let num_var_3 = var_store.fresh();
    let ret_var = var_store.fresh();
    let record_var = var_store.fresh();

    // let arg_3 = RunLowLevel NumSubChecked arg_1 arg_2
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

    // arg_3 = RunLowLevel NumSubChecked arg_1 arg_2
    let def = crate::def::Def {
        loc_pattern: no_region(Pattern::Identifier(Symbol::ARG_3)),
        loc_expr: no_region(RunLowLevel {
            op: LowLevel::NumSubChecked,
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

/// Num.mul : Num a, Num a -> Num a
fn num_mul(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumMul)
}

/// Num.mulWrap : Int, Int -> Int
fn num_mul_wrap(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumMulWrap)
}

/// Num.mulChecked : Num a, Num a -> Result (Num a) [ Overflow ]*
fn num_mul_checked(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let num_var_1 = var_store.fresh();
    let num_var_2 = var_store.fresh();
    let num_var_3 = var_store.fresh();
    let ret_var = var_store.fresh();
    let record_var = var_store.fresh();

    // let arg_3 = RunLowLevel NumMulChecked arg_1 arg_2
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

    // arg_3 = RunLowLevel NumMulChecked arg_1 arg_2
    let def = crate::def::Def {
        loc_pattern: no_region(Pattern::Identifier(Symbol::ARG_3)),
        loc_expr: no_region(RunLowLevel {
            op: LowLevel::NumMulChecked,
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
            (arg_var, Num(unbound_zero_var, 0)),
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
            (arg_var, Num(unbound_zero_var, 0)),
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
            (arg_var, Num(unbound_zero_var, 0)),
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
            (arg_var, Int(var_store.fresh(), var_store.fresh(), 1)),
            (
                arg_var,
                RunLowLevel {
                    op: LowLevel::NumRemUnchecked,
                    args: vec![
                        (arg_var, Var(Symbol::ARG_1)),
                        (arg_var, Num(unbound_two_var, 2)),
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
            (arg_var, Num(arg_num_var, 0)),
            (
                arg_var,
                RunLowLevel {
                    op: LowLevel::NumRemUnchecked,
                    args: vec![
                        (arg_var, Var(Symbol::ARG_1)),
                        (arg_var, Num(arg_num_var, 2)),
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
    let percision_var = var_store.fresh();
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
                        (float_var, Var(Symbol::ARG_1)),
                        (float_var, Float(unbound_zero_var, percision_var, 0.0)),
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
                            op: LowLevel::NumSqrtUnchecked,
                            args: vec![(float_var, Var(Symbol::ARG_1))],
                            ret_var: float_var,
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

/// Num.powInt : Int, Int -> Int
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

/// Num.bitwiseAnd : Int, Int -> Int
fn num_bitwise_and(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumBitwiseAnd)
}

/// Num.bitwiseXor : Int, Int -> Int
fn num_bitwise_xor(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumBitwiseXor)
}

/// Num.bitwiseOr: Int, Int -> Int
fn num_bitwise_or(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumBitwiseOr)
}

/// Num.shiftLeftBy: Nat, Int a -> Int a
fn num_shift_left_by(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::NumShiftLeftBy, var_store)
}

/// List.isEmpty : List * -> Bool
fn list_is_empty(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let bool_var = var_store.fresh();
    let len_var = var_store.fresh();
    let unbound_zero_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::Eq,
        args: vec![
            (len_var, Num(unbound_zero_var, 0)),
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
    let str_var = var_store.fresh();
    let bool_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::StrStartsWith,
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

/// Str.fromInt : Int * -> Str
fn str_from_int(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let int_var = var_store.fresh();
    let str_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::StrFromInt,
        args: vec![(int_var, Var(Symbol::ARG_1))],
        ret_var: str_var,
    };

    defn(
        symbol,
        vec![(int_var, Symbol::ARG_1)],
        var_store,
        body,
        str_var,
    )
}

/// Str.fromUtf8 : List U8 -> Result Str [ BadUtf8 Utf8Problem ]*
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
                    field: "isOk".into(),
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
                    field: "str".into(),
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
                            field: "problem".into(),
                            field_var: var_store.fresh(),
                            loc_expr: Box::new(no_region(Var(Symbol::ARG_2))),
                        },
                        Access {
                            record_var,
                            ext_var: var_store.fresh(),
                            field: "byteIndex".into(),
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

/// Str.fromFloat : Float * -> Str
fn str_from_float(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let float_var = var_store.fresh();
    let str_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::StrFromFloat,
        args: vec![(float_var, Var(Symbol::ARG_1))],
        ret_var: str_var,
    };

    defn(
        symbol,
        vec![(float_var, Symbol::ARG_1)],
        var_store,
        body,
        str_var,
    )
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

/// List.repeat : elem, Int -> List elem
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

/// List.set : List elem, Int, elem -> List elem
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

/// List.walk : List elem, (elem -> accum -> accum), accum -> accum
fn list_walk(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let func_var = var_store.fresh();
    let accum_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListWalk,
        args: vec![
            (list_var, Var(Symbol::ARG_1)),
            (func_var, Var(Symbol::ARG_2)),
            (accum_var, Var(Symbol::ARG_3)),
        ],
        ret_var: accum_var,
    };

    defn(
        symbol,
        vec![
            (list_var, Symbol::ARG_1),
            (func_var, Symbol::ARG_2),
            (accum_var, Symbol::ARG_3),
        ],
        var_store,
        body,
        accum_var,
    )
}

/// List.walkBackwards : List elem, (elem -> accum -> accum), accum -> accum
fn list_walk_backwards(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let func_var = var_store.fresh();
    let accum_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListWalkBackwards,
        args: vec![
            (list_var, Var(Symbol::ARG_1)),
            (func_var, Var(Symbol::ARG_2)),
            (accum_var, Var(Symbol::ARG_3)),
        ],
        ret_var: accum_var,
    };

    defn(
        symbol,
        vec![
            (list_var, Symbol::ARG_1),
            (func_var, Symbol::ARG_2),
            (accum_var, Symbol::ARG_3),
        ],
        var_store,
        body,
        accum_var,
    )
}

/// List.sum : List (Num a) -> Num a
fn list_sum(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let result_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListSum,
        args: vec![(list_var, Var(Symbol::ARG_1))],
        ret_var: result_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1)],
        var_store,
        body,
        result_var,
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

/// List.map : List before, (before -> after) -> List after
fn list_map(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::ListMap, var_store)
}

/// List.mapWithIndex : List before, (Nat, before -> after) -> List after
fn list_map_with_index(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::ListMapWithIndex, var_store)
}

/// Dict.hashTestOnly : k, v -> Nat
pub fn dict_hash_test_only(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_2(symbol, LowLevel::Hash, var_store)
}

/// Dict.len : Dict * * -> Nat
fn dict_len(symbol: Symbol, var_store: &mut VarStore) -> Def {
    lowlevel_1(symbol, LowLevel::DictSize, var_store)
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
        loc_expr: Located::at_zero(body),
        loc_pattern: Located::at_zero(Pattern::Identifier(symbol)),
        pattern_vars: SendMap::default(),
    }
}

/// Dict.singleton : k, v -> Dict k v
fn dict_singleton(symbol: Symbol, var_store: &mut VarStore) -> Def {
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
        loc_expr: Located::at_zero(def_body),
        loc_pattern: Located::at_zero(Pattern::Identifier(temp_record)),
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
        vec![tag("OutOfBounds", Vec::new(), var_store)],
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

/// Dict.walk : Dict k v, (k, v, accum -> accum), accum -> accum
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
        loc_expr: Located::at_zero(body),
        loc_pattern: Located::at_zero(Pattern::Identifier(symbol)),
        pattern_vars: SendMap::default(),
    }
}

/// Set.singleton : k -> Set k
fn set_singleton(symbol: Symbol, var_store: &mut VarStore) -> Def {
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

/// Set.walk : Set k,  (k, accum -> accum), accum -> accum
fn set_walk(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let dict_var = var_store.fresh();
    let func_var = var_store.fresh();
    let key_var = var_store.fresh();
    let accum_var = var_store.fresh();
    let wrapper_var = var_store.fresh();

    let user_function = Box::new((
        func_var,
        no_region(Var(Symbol::ARG_2)),
        var_store.fresh(),
        accum_var,
    ));

    let call_func = Call(
        user_function,
        vec![
            (key_var, no_region(Var(Symbol::ARG_5))),
            (accum_var, no_region(Var(Symbol::ARG_6))),
        ],
        CalledVia::Space,
    );

    let wrapper = Closure {
        function_type: wrapper_var,
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
        return_type: accum_var,
        name: Symbol::SET_WALK_USER_FUNCTION,
        recursive: Recursive::NotRecursive,
        captured_symbols: vec![(Symbol::ARG_2, func_var)],
        arguments: vec![
            (key_var, no_region(Pattern::Identifier(Symbol::ARG_5))),
            (Variable::EMPTY_RECORD, no_region(Pattern::Underscore)),
            (accum_var, no_region(Pattern::Identifier(Symbol::ARG_6))),
        ],
        loc_body: Box::new(no_region(call_func)),
    };

    let body = RunLowLevel {
        op: LowLevel::DictWalk,
        args: vec![
            (dict_var, Var(Symbol::ARG_1)),
            (wrapper_var, wrapper),
            (accum_var, Var(Symbol::ARG_3)),
        ],
        ret_var: accum_var,
    };

    defn(
        symbol,
        vec![
            (dict_var, Symbol::ARG_1),
            (func_var, Symbol::ARG_2),
            (accum_var, Symbol::ARG_3),
        ],
        var_store,
        body,
        accum_var,
    )
}

/// Num.rem : Int, Int -> Result Int [ DivByZero ]*
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
                        (num_var, Num(unbound_zero_var, 0)),
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
    let percision_var = var_store.fresh();
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
                        (num_var, Float(unbound_zero_var, percision_var, 0.0)),
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

/// Num.div : Int, Int -> Result Int [ DivByZero ]*
fn num_div_int(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let num_var = var_store.fresh();
    let unbound_zero_var = var_store.fresh();
    let unbound_zero_percision_var = var_store.fresh();
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
                            Int(unbound_zero_var, unbound_zero_percision_var, 0),
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

/// List.first : List elem -> Result elem [ ListWasEmpty ]*
///
/// List.first :
///     Attr (* | u) (List (Attr u a)),
///     -> Attr * (Result (Attr u a) (Attr * [ OutOfBounds ]*))
fn list_first(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let list_var = var_store.fresh();
    let len_var = var_store.fresh();
    let zero_var = var_store.fresh();
    let zero_percision_var = var_store.fresh();
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
                        (len_var, Int(zero_var, zero_percision_var, 0)),
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
                                (len_var, Int(zero_var, zero_percision_var, 0)),
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
    let len_var = var_store.fresh();
    let num_var = var_store.fresh();
    let num_percision_var = var_store.fresh();
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
                        (len_var, Int(num_var, num_percision_var, 0)),
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
                                            (arg_var, Int(num_var, num_percision_var, 1)),
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

        let branch = WhenBranch {
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

        let branch = WhenBranch {
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

        let branch = WhenBranch {
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

        let branch = WhenBranch {
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

        let branch = WhenBranch {
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

        let branch = WhenBranch {
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

#[inline(always)]
fn no_region<T>(value: T) -> Located<T> {
    Located {
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
            .collect::<Vec<(Variable, Located<Expr>)>>(),
    }
}

// #[inline(always)]
// fn record(fields: Vec<(Lowercase, Field)>, var_store: &mut VarStore) -> Expr {
// let mut send_map = SendMap::default();
// for (k, v) in fields {
// send_map.insert(k, v);
// }
// Expr::Record {
// record_var: var_store.fresh(),
// fields: send_map,
// }
// }

#[inline(always)]
fn defn(
    fn_name: Symbol,
    args: Vec<(Variable, Symbol)>,
    var_store: &mut VarStore,
    body: Expr,
    ret_var: Variable,
) -> Def {
    use crate::pattern::Pattern::*;

    let closure_args = args
        .into_iter()
        .map(|(var, symbol)| (var, no_region(Identifier(symbol))))
        .collect();

    let expr = Closure {
        function_type: var_store.fresh(),
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
        return_type: ret_var,
        name: fn_name,
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments: closure_args,
        loc_body: Box::new(no_region(body)),
    };

    Def {
        loc_pattern: Located {
            region: Region::zero(),
            value: Pattern::Identifier(fn_name),
        },
        loc_expr: Located {
            region: Region::zero(),
            value: expr,
        },
        expr_var: var_store.fresh(),
        pattern_vars: SendMap::default(),
        annotation: None,
    }
}
