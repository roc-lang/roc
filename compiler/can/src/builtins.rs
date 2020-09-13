use crate::def::Def;
use crate::expr::Expr::*;
use crate::expr::{Expr, Recursive};
use crate::pattern::Pattern;
use roc_collections::all::{MutMap, SendMap};
use roc_module::ident::TagName;
use roc_module::low_level::LowLevel;
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
        Symbol::STR_IS_EMPTY => str_is_empty,
        Symbol::LIST_LEN => list_len,
        Symbol::LIST_GET => list_get,
        Symbol::LIST_SET => list_set,
        Symbol::LIST_APPEND => list_append,
        Symbol::LIST_FIRST => list_first,
        Symbol::LIST_IS_EMPTY => list_is_empty,
        Symbol::LIST_SINGLE => list_single,
        Symbol::LIST_REPEAT => list_repeat,
        Symbol::LIST_REVERSE => list_reverse,
        Symbol::LIST_CONCAT => list_concat,
        Symbol::LIST_PREPEND => list_prepend,
        Symbol::LIST_JOIN => list_join,
        Symbol::LIST_MAP => list_map,
        Symbol::LIST_KEEP_IF => list_keep_if,
        Symbol::LIST_WALK_RIGHT => list_walk_right,
        Symbol::NUM_ADD => num_add,
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

/// Num a, Num a -> Bool
fn num_bool_binop(symbol: Symbol, var_store: &mut VarStore, op: LowLevel) -> Def {
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

/// Num.sub : Num a, Num a -> Num a
fn num_sub(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumSub)
}

/// Num.mul : Num a, Num a -> Num a
fn num_mul(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumMul)
}

/// Num.isGt : Num a, Num a -> Bool
fn num_gt(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_bool_binop(symbol, var_store, LowLevel::NumGt)
}

/// Num.isGte : Num a, Num a -> Bool
fn num_gte(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_bool_binop(symbol, var_store, LowLevel::NumGte)
}

/// Num.isLt : Num a, Num a -> Bool
fn num_lt(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_bool_binop(symbol, var_store, LowLevel::NumLt)
}

/// Num.isLte : Num a, Num a -> Num a
fn num_lte(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_bool_binop(symbol, var_store, LowLevel::NumLte)
}

/// Num.compare : Num a, Num a -> [ LT, EQ, GT ]
fn num_compare(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_bool_binop(symbol, var_store, LowLevel::NumCompare)
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
            (arg_var, Int(var_store.fresh(), 1)),
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
                        (float_var, Float(unbound_zero_var, 0.0)),
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

/// Str.isEmpty : List * -> Bool
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

/// List.walkRight : List elem, (elem -> accum -> accum), accum -> accum
fn list_walk_right(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let func_var = var_store.fresh();
    let accum_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListWalkRight,
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

/// List.map : List before, (before -> after) -> List after
fn list_map(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let list_var = var_store.fresh();
    let func_var = var_store.fresh();
    let ret_list_var = var_store.fresh();

    let body = RunLowLevel {
        op: LowLevel::ListMap,
        args: vec![
            (list_var, Var(Symbol::ARG_1)),
            (func_var, Var(Symbol::ARG_2)),
        ],
        ret_var: ret_list_var,
    };

    defn(
        symbol,
        vec![(list_var, Symbol::ARG_1), (func_var, Symbol::ARG_2)],
        var_store,
        body,
        ret_list_var,
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
                        (num_var, Float(unbound_zero_var, 0.0)),
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
                        (num_var, Int(unbound_zero_var, 0)),
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
                        (len_var, Int(zero_var, 0)),
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
                            args: vec![(list_var, Var(Symbol::ARG_1)), (len_var, Int(zero_var, 0))],
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

    let expr = Closure(
        var_store.fresh(),
        fn_name,
        Recursive::NotRecursive,
        closure_args,
        Box::new((no_region(body), ret_var)),
    );

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
