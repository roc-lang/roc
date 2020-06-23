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
        Symbol::LIST_LEN => list_len,
        Symbol::LIST_GET => list_get,
        Symbol::LIST_SET => list_set,
        Symbol::LIST_FIRST => list_first,
        Symbol::LIST_IS_EMPTY => list_is_empty,
        Symbol::NUM_ADD => num_add,
        Symbol::NUM_SUB => num_sub,
        Symbol::NUM_MUL => num_mul,
        Symbol::NUM_GT => num_gt,
        Symbol::NUM_GTE => num_gte,
        Symbol::NUM_LT => num_lt,
        Symbol::NUM_LTE => num_lte,
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
    let bool_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::Eq,
        args: vec![
            (bool_var, Var(Symbol::BOOL_BINOP_LHS)),
            (bool_var, Var(Symbol::BOOL_BINOP_RHS)),
        ],
        ret_var: var_store.fresh(),
    };

    defn(
        symbol,
        vec![Symbol::BOOL_BINOP_LHS, Symbol::BOOL_BINOP_RHS],
        var_store,
        body,
    )
}

/// Bool.isNotEq : val, val -> Bool
fn bool_neq(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::NotEq,
        args: vec![
            (bool_var, Var(Symbol::BOOL_BINOP_LHS)),
            (bool_var, Var(Symbol::BOOL_BINOP_RHS)),
        ],
        ret_var: var_store.fresh(),
    };

    defn(
        symbol,
        vec![Symbol::BOOL_BINOP_LHS, Symbol::BOOL_BINOP_RHS],
        var_store,
        body,
    )
}

/// Bool.or : val, val -> Bool
fn bool_or(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let body = RunLowLevel {
        op: LowLevel::Or,
        args: vec![
            (var_store.fresh(), Var(Symbol::BOOL_BINOP_LHS)),
            (var_store.fresh(), Var(Symbol::BOOL_BINOP_RHS)),
        ],
        ret_var: var_store.fresh(),
    };

    defn(
        symbol,
        vec![Symbol::BOOL_BINOP_LHS, Symbol::BOOL_BINOP_RHS],
        var_store,
        body,
    )
}

/// Bool.not : Bool -> Bool
fn bool_not(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let body = RunLowLevel {
        op: LowLevel::Not,
        args: vec![(var_store.fresh(), Var(Symbol::BOOL_BINOP_LHS))],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::BOOL_BINOP_LHS], var_store, body)
}

/// Bool.and : val, val -> Bool
fn bool_and(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let body = RunLowLevel {
        op: LowLevel::And,
        args: vec![
            (var_store.fresh(), Var(Symbol::BOOL_BINOP_LHS)),
            (var_store.fresh(), Var(Symbol::BOOL_BINOP_RHS)),
        ],
        ret_var: var_store.fresh(),
    };

    defn(
        symbol,
        vec![Symbol::BOOL_BINOP_LHS, Symbol::BOOL_BINOP_RHS],
        var_store,
        body,
    )
}

fn num_binop(symbol: Symbol, var_store: &mut VarStore, op: LowLevel) -> Def {
    let body = RunLowLevel {
        op,
        args: vec![
            (var_store.fresh(), Var(Symbol::ARG_1)),
            (var_store.fresh(), Var(Symbol::ARG_2)),
        ],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::ARG_1, Symbol::ARG_2], var_store, body)
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

/// Num.isGt : Num a, Num a -> Num a
fn num_gt(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumGt)
}

/// Num.isGte : Num a, Num a -> Num a
fn num_gte(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumGte)
}

/// Num.isLt : Num a, Num a -> Num a
fn num_lt(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumLt)
}

/// Num.isLte : Num a, Num a -> Num a
fn num_lte(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumLte)
}

/// Num.sin : Float -> Float
fn num_sin(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::NumSin,
        args: vec![(arg_var, Var(Symbol::ARG_1))],
        ret_var: arg_var,
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
}

/// Num.cos : Float -> Float
fn num_cos(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::NumCos,
        args: vec![(arg_var, Var(Symbol::ARG_1))],
        ret_var: arg_var,
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
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

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
}

/// Num.isZero : Float -> Bool
fn num_is_zero(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::Eq,
        args: vec![
            (arg_var, Var(Symbol::ARG_1)),
            (arg_var, Num(var_store.fresh(), 0)),
        ],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
}

/// Num.isNegative : Float -> Bool
fn num_is_negative(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::NumGt,
        args: vec![
            (arg_var, Num(var_store.fresh(), 0)),
            (arg_var, Var(Symbol::ARG_1)),
        ],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
}

/// Num.isPositive : Float -> Bool
fn num_is_positive(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::NumGt,
        args: vec![
            (arg_var, Var(Symbol::ARG_1)),
            (arg_var, Num(var_store.fresh(), 0)),
        ],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
}

/// Num.isOdd : Int -> Bool
fn num_is_odd(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::Eq,
        args: vec![
            (bool_var, Int(var_store.fresh(), 1)),
            (
                bool_var,
                RunLowLevel {
                    op: LowLevel::NumRemUnchecked,
                    args: vec![
                        (var_store.fresh(), Var(Symbol::ARG_1)),
                        (var_store.fresh(), Int(var_store.fresh(), 2)),
                    ],
                    ret_var: var_store.fresh(),
                },
            ),
        ],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
}

/// Num.isEven : Num * -> Bool
fn num_is_even(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let arg_num_var = var_store.fresh();
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
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
}

/// Num.toFloat : Num * -> Float
fn num_to_float(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let body = RunLowLevel {
        op: LowLevel::NumToFloat,
        args: vec![(var_store.fresh(), Var(Symbol::ARG_1))],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
}

/// Num.sqrt : Float -> Result Float [ SqrtOfNegative ]*
fn num_sqrt(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let body = RunLowLevel {
        op: LowLevel::NumSqrt,
        args: vec![(var_store.fresh(), Var(Symbol::ARG_1))],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
}

/// Num.round : Float -> Int
fn num_round(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let body = RunLowLevel {
        op: LowLevel::NumRound,
        args: vec![(var_store.fresh(), Var(Symbol::ARG_1))],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
}

/// List.isEmpty : List * -> Bool
fn list_is_empty(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::Eq,
        args: vec![
            (bool_var, Num(var_store.fresh(), 0)),
            (
                bool_var,
                RunLowLevel {
                    op: LowLevel::ListLen,
                    args: vec![(var_store.fresh(), Var(Symbol::ARG_1))],
                    ret_var: var_store.fresh(),
                },
            ),
        ],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
}

/// List.len : List * -> Int
fn list_len(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let body = RunLowLevel {
        op: LowLevel::ListLen,
        args: vec![(var_store.fresh(), Var(Symbol::ARG_1))],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
}

/// List.get : List elem, Int -> Result elem [ OutOfBounds ]*
fn list_get(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_list = Symbol::ARG_1;
    let arg_index = Symbol::ARG_2;

    // Perform a bounds check. If it passes, run LowLevel::ListGetUnsafe
    let body = If {
        cond_var: var_store.fresh(),
        branch_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // index < List.len list
                RunLowLevel {
                    op: LowLevel::NumLt,
                    args: vec![
                        (var_store.fresh(), Var(arg_index)),
                        (
                            var_store.fresh(),
                            RunLowLevel {
                                op: LowLevel::ListLen,
                                args: vec![(var_store.fresh(), Var(arg_list))],
                                ret_var: var_store.fresh(),
                            },
                        ),
                    ],
                    ret_var: var_store.fresh(),
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
                            args: vec![
                                (var_store.fresh(), Var(arg_list)),
                                (var_store.fresh(), Var(arg_index)),
                            ],
                            ret_var: var_store.fresh(),
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

    defn(symbol, vec![Symbol::ARG_1, Symbol::ARG_2], var_store, body)
}

/// List.set : List elem, Int, elem -> List elem
fn list_set(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_list = Symbol::ARG_1;
    let arg_index = Symbol::ARG_2;
    let arg_elem = Symbol::ARG_3;

    // Perform a bounds check. If it passes, run LowLevel::ListSet.
    // Otherwise, return the list unmodified.
    let body = If {
        cond_var: var_store.fresh(),
        branch_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // index < List.len list
                RunLowLevel {
                    op: LowLevel::NumLt,
                    args: vec![
                        (var_store.fresh(), Var(arg_index)),
                        (
                            var_store.fresh(),
                            RunLowLevel {
                                op: LowLevel::ListLen,
                                args: vec![(var_store.fresh(), Var(arg_list))],
                                ret_var: var_store.fresh(),
                            },
                        ),
                    ],
                    ret_var: var_store.fresh(),
                },
            ),
            // then-branch
            no_region(
                // List.setUnsafe list index
                RunLowLevel {
                    op: LowLevel::ListSet,
                    args: vec![
                        (var_store.fresh(), Var(arg_list)),
                        (var_store.fresh(), Var(arg_index)),
                        (var_store.fresh(), Var(arg_elem)),
                    ],
                    ret_var: var_store.fresh(),
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
        vec![Symbol::ARG_1, Symbol::ARG_2, Symbol::ARG_3],
        var_store,
        body,
    )
}

/// Num.rem : Int, Int -> Result Int [ DivByZero ]*
fn num_rem(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let body = If {
        branch_var: var_store.fresh(),
        cond_var: var_store.fresh(),
        branches: vec![(
            // if condition
            no_region(
                // Num.neq arg1 0
                RunLowLevel {
                    op: LowLevel::NotEq,
                    args: vec![
                        (bool_var, Var(Symbol::ARG_2)),
                        (bool_var, Int(var_store.fresh(), 0)),
                    ],
                    ret_var: var_store.fresh(),
                },
            ),
            // arg1 was not zero
            no_region(
                // Ok (Int.#remUnsafe arg0 arg1)
                tag(
                    "Ok",
                    vec![
                        // Num.#remUnsafe arg0 arg1
                        RunLowLevel {
                            op: LowLevel::NumRemUnchecked,
                            args: vec![
                                (var_store.fresh(), Var(Symbol::ARG_1)),
                                (var_store.fresh(), Var(Symbol::ARG_2)),
                            ],
                            ret_var: var_store.fresh(),
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

    defn(symbol, vec![Symbol::ARG_1, Symbol::ARG_2], var_store, body)
}

/// Num.neg : Num a -> Num a
fn num_neg(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let body = RunLowLevel {
        op: LowLevel::NumNeg,
        args: vec![(var_store.fresh(), Var(Symbol::ARG_1))],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
}

/// Num.abs : Num a -> Num a
fn num_abs(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let arg_var = var_store.fresh();
    let body = RunLowLevel {
        op: LowLevel::NumAbs,
        args: vec![(arg_var, Var(Symbol::ARG_1))],
        ret_var: arg_var,
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
}

/// Num.div : Float, Float -> Result Float [ DivByZero ]*
fn num_div_float(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let body = If {
        branch_var: var_store.fresh(),
        cond_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // Num.neq denominator 0
                RunLowLevel {
                    op: LowLevel::NotEq,
                    args: vec![
                        (bool_var, Var(Symbol::ARG_1)),
                        (bool_var, Float(var_store.fresh(), 0.0)),
                    ],
                    ret_var: var_store.fresh(),
                },
            ),
            // denominator was not zero
            no_region(
                // Ok (Float.#divUnsafe numerator denominator)
                tag(
                    "Ok",
                    vec![
                        // Num.#divUnsafe numerator denominator
                        RunLowLevel {
                            op: LowLevel::NumDivUnchecked,
                            args: vec![
                                (var_store.fresh(), Var(Symbol::ARG_1)),
                                (var_store.fresh(), Var(Symbol::ARG_2)),
                            ],
                            ret_var: var_store.fresh(),
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

    defn(symbol, vec![Symbol::ARG_1, Symbol::ARG_2], var_store, body)
}

/// Num.div : Int, Int -> Result Int [ DivByZero ]*
fn num_div_int(symbol: Symbol, var_store: &mut VarStore) -> Def {
    let bool_var = var_store.fresh();
    let body = If {
        branch_var: var_store.fresh(),
        cond_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // Num.neq denominator 0
                RunLowLevel {
                    op: LowLevel::NotEq,
                    args: vec![
                        (bool_var, Var(Symbol::ARG_1)),
                        (bool_var, Int(var_store.fresh(), 0)),
                    ],
                    ret_var: var_store.fresh(),
                },
            ),
            // denominator was not zero
            no_region(
                // Ok (Int.#divUnsafe numerator denominator)
                tag(
                    "Ok",
                    vec![
                        // Num.#divUnsafe numerator denominator
                        RunLowLevel {
                            op: LowLevel::NumDivUnchecked,
                            args: vec![
                                (var_store.fresh(), Var(Symbol::ARG_1)),
                                (var_store.fresh(), Var(Symbol::ARG_2)),
                            ],
                            ret_var: var_store.fresh(),
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

    defn(symbol, vec![Symbol::ARG_1, Symbol::ARG_2], var_store, body)
}

/// List.first : List elem -> Result elem [ ListWasEmpty ]*
fn list_first(symbol: Symbol, var_store: &mut VarStore) -> Def {
    // Perform a bounds check. If it passes, delegate to List.getUnsafe.
    let body = If {
        // TODO Use "when" instead of "if" so that we can have False be the first branch.
        // We want that for branch prediction; usually we expect the list to be nonempty.
        cond_var: var_store.fresh(),
        branch_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // List.isEmpty list
                RunLowLevel {
                    op: LowLevel::ListIsEmpty,
                    args: vec![(var_store.fresh(), Var(Symbol::ARG_1))],
                    ret_var: var_store.fresh(),
                },
            ),
            // list was empty
            no_region(
                // Err ListWasEmpty
                tag(
                    "Err",
                    vec![tag("ListWasEmpty", Vec::new(), var_store)],
                    var_store,
                ),
            ),
        )],
        final_else: Box::new(
            // list was not empty
            no_region(
                // Ok (List.#getUnsafe list 0)
                tag(
                    "Ok",
                    vec![
                        // List#getUnsafe list 0
                        RunLowLevel {
                            op: LowLevel::ListGetUnsafe,
                            args: vec![
                                (var_store.fresh(), Var(Symbol::ARG_1)),
                                (var_store.fresh(), Int(var_store.fresh(), 0)),
                            ],
                            ret_var: var_store.fresh(),
                        },
                    ],
                    var_store,
                ),
            ),
        ),
    };

    defn(symbol, vec![Symbol::ARG_1], var_store, body)
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
fn defn(fn_name: Symbol, args: Vec<Symbol>, var_store: &mut VarStore, body: Expr) -> Def {
    use crate::pattern::Pattern::*;

    let closure_args = args
        .into_iter()
        .map(|symbol| (var_store.fresh(), no_region(Identifier(symbol))))
        .collect();

    let expr = Closure(
        var_store.fresh(),
        fn_name,
        Recursive::NotRecursive,
        closure_args,
        Box::new((no_region(body), var_store.fresh())),
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
