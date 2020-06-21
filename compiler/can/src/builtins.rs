use crate::def::Def;
use crate::expr::{Expr, Recursive};
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
        Symbol::NUM_ADD => num_add,
        Symbol::NUM_SUB => num_sub,
        Symbol::NUM_MUL => num_mul,
        Symbol::NUM_GT => num_gt,
        Symbol::NUM_GTE => num_gte,
        Symbol::NUM_LT => num_lt,
        Symbol::NUM_LTE => num_lte,
        Symbol::INT_DIV => int_div,
        Symbol::INT_ABS => int_abs,
        Symbol::INT_REM => int_rem,
        Symbol::INT_IS_ODD => int_is_odd,
        Symbol::INT_IS_EVEN => int_is_even,
        Symbol::INT_IS_ZERO => int_is_zero,
        Symbol::INT_IS_POSITIVE => int_is_positive,
        Symbol::INT_IS_NEGATIVE => int_is_negative,
        Symbol::FLOAT_IS_POSITIVE => float_is_positive,
        Symbol::FLOAT_IS_NEGATIVE => float_is_negative,
        Symbol::FLOAT_IS_ZERO => float_is_zero,
        Symbol::FLOAT_TAN => float_tan,
    }
}

/// Bool.isEq : val, val -> Bool
fn bool_eq(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    let body = RunLowLevel {
        op: LowLevel::Eq,
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

/// Bool.isNotEq : val, val -> Bool
fn bool_neq(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    let body = RunLowLevel {
        op: LowLevel::NotEq,
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

/// Bool.or : val, val -> Bool
fn bool_or(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

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
    use crate::expr::Expr::*;

    let body = RunLowLevel {
        op: LowLevel::Not,
        args: vec![(var_store.fresh(), Var(Symbol::BOOL_BINOP_LHS))],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::BOOL_BINOP_LHS], var_store, body)
}

/// Bool.and : val, val -> Bool
fn bool_and(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

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
    use crate::expr::Expr::*;

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

/// Num.gt : Num a, Num a -> Num a
fn num_gt(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumGt)
}

/// Num.gte : Num a, Num a -> Num a
fn num_gte(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumGte)
}

/// Num.lt : Num a, Num a -> Num a
fn num_lt(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumLt)
}

/// Num.lte : Num a, Num a -> Num a
fn num_lte(symbol: Symbol, var_store: &mut VarStore) -> Def {
    num_binop(symbol, var_store, LowLevel::NumLte)
}

/// Float.tan : Float -> Float
fn float_tan(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    let body = call(
        Symbol::FLOAT_DIV,
        vec![
            call(
                Symbol::FLOAT_SIN,
                vec![Var(Symbol::FLOAT_TAN_ARG)],
                var_store,
            ),
            call(
                Symbol::FLOAT_COS,
                vec![Var(Symbol::FLOAT_TAN_ARG)],
                var_store,
            ),
        ],
        var_store,
    );

    defn(symbol, vec![Symbol::FLOAT_TAN_ARG], var_store, body)
}

/// Float.isZero : Float -> Bool
fn float_is_zero(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    let body = call(
        Symbol::FLOAT_EQ,
        vec![
            Float(var_store.fresh(), 0.0),
            Var(Symbol::FLOAT_IS_ZERO_ARG),
        ],
        var_store,
    );

    defn(symbol, vec![Symbol::FLOAT_IS_ZERO_ARG], var_store, body)
}

/// Float.isNegative : Float -> Bool
fn float_is_negative(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    let body = call(
        Symbol::FLOAT_GT,
        vec![
            Float(var_store.fresh(), 0.0),
            Var(Symbol::FLOAT_IS_NEGATIVE_ARG),
        ],
        var_store,
    );

    defn(symbol, vec![Symbol::FLOAT_IS_NEGATIVE_ARG], var_store, body)
}

/// Float.isPositive : Float -> Bool
fn float_is_positive(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    let body = call(
        Symbol::FLOAT_GT,
        vec![
            Var(Symbol::FLOAT_IS_POSITIVE_ARG),
            Float(var_store.fresh(), 0.0),
        ],
        var_store,
    );

    defn(symbol, vec![Symbol::FLOAT_IS_POSITIVE_ARG], var_store, body)
}

/// Int.isNegative : Int -> Bool
fn int_is_negative(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    let body = call(
        Symbol::NUM_LT,
        vec![Var(Symbol::INT_IS_NEGATIVE_ARG), Int(var_store.fresh(), 0)],
        var_store,
    );

    defn(symbol, vec![Symbol::INT_IS_NEGATIVE_ARG], var_store, body)
}

/// Int.isPositive : Int -> Bool
fn int_is_positive(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    let body = call(
        Symbol::NUM_GT,
        vec![Var(Symbol::INT_IS_POSITIVE_ARG), Int(var_store.fresh(), 0)],
        var_store,
    );

    defn(symbol, vec![Symbol::INT_IS_POSITIVE_ARG], var_store, body)
}

/// Int.isZero : Int -> Bool
fn int_is_zero(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    let body = RunLowLevel {
        op: LowLevel::Eq,
        args: vec![
            (var_store.fresh(), Var(Symbol::INT_IS_ZERO_ARG)),
            (var_store.fresh(), Int(var_store.fresh(), 0)),
        ],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::INT_IS_ZERO_ARG], var_store, body)
}

/// Int.isOdd : Int -> Bool
fn int_is_odd(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    let body = RunLowLevel {
        op: LowLevel::Eq,
        args: vec![
            (
                var_store.fresh(),
                call(
                    Symbol::INT_REM_UNSAFE,
                    vec![Var(Symbol::INT_IS_ODD_ARG), Int(var_store.fresh(), 2)],
                    var_store,
                ),
            ),
            (var_store.fresh(), Int(var_store.fresh(), 1)),
        ],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::INT_IS_ODD_ARG], var_store, body)
}

/// Int.isEven : Int -> Bool
fn int_is_even(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    let body = RunLowLevel {
        op: LowLevel::Eq,
        args: vec![
            (var_store.fresh(), Var(Symbol::INT_IS_EVEN_ARG)),
            (var_store.fresh(), Int(var_store.fresh(), 2)),
        ],
        ret_var: var_store.fresh(),
    };

    defn(symbol, vec![Symbol::INT_IS_EVEN_ARG], var_store, body)
}

/// List.len : List * -> Int
fn list_len(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    // Polymorphic wrapper around LowLevel::ListLen
    let arg = Symbol::LIST_LEN_ARG;
    let arg_var = var_store.fresh();
    let ret_var = var_store.fresh();

    defn(
        symbol,
        vec![arg],
        var_store,
        RunLowLevel {
            op: LowLevel::ListLen,
            args: vec![(arg_var, Var(arg))],
            ret_var,
        },
    )
}

/// List.get : List elem, Int -> Result elem [ OutOfBounds ]*
fn list_get(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    // Perform a bounds check. If it passes, run LowLevel::ListGetUnsafe
    let body = If {
        cond_var: var_store.fresh(),
        branch_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // index < List.len list
                call(
                    Symbol::NUM_LT,
                    vec![
                        Var(Symbol::LIST_GET_ARG_INDEX),
                        call(
                            Symbol::LIST_LEN,
                            vec![Var(Symbol::LIST_GET_ARG_LIST)],
                            var_store,
                        ),
                    ],
                    var_store,
                ),
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
                                (var_store.fresh(), Var(Symbol::LIST_GET_ARG_LIST)),
                                (var_store.fresh(), Var(Symbol::LIST_GET_ARG_INDEX)),
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

    defn(
        symbol,
        vec![Symbol::LIST_GET_ARG_LIST, Symbol::LIST_GET_ARG_INDEX],
        var_store,
        body,
    )
}

/// List.set : List elem, Int, elem -> List elem
fn list_set(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    // Perform a bounds check. If it passes, run LowLevel::ListSetUnsafe.
    // Otherwise, return the list unmodified.
    let body = If {
        cond_var: var_store.fresh(),
        branch_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // index < List.len list
                call(
                    Symbol::NUM_LT,
                    vec![
                        Var(Symbol::LIST_SET_ARG_INDEX),
                        RunLowLevel {
                            op: LowLevel::ListLen,
                            args: vec![(var_store.fresh(), Var(Symbol::LIST_SET_ARG_LIST))],
                            ret_var: var_store.fresh(),
                        },
                    ],
                    var_store,
                ),
            ),
            // then-branch
            no_region(
                // List.setUnsafe list index
                RunLowLevel {
                    op: LowLevel::ListSetUnsafe,
                    args: vec![
                        (var_store.fresh(), Var(Symbol::LIST_SET_ARG_LIST)),
                        (var_store.fresh(), Var(Symbol::LIST_SET_ARG_INDEX)),
                        (var_store.fresh(), Var(Symbol::LIST_SET_ARG_ELEM)),
                    ],
                    ret_var: var_store.fresh(),
                },
            ),
        )],
        final_else: Box::new(
            // else-branch
            no_region(Var(Symbol::LIST_SET_ARG_LIST)),
        ),
    };

    defn(
        symbol,
        vec![Symbol::LIST_GET_ARG_LIST, Symbol::LIST_GET_ARG_INDEX],
        var_store,
        body,
    )
}

/// Int.rem : Int, Int -> Int
fn int_rem(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    let body = If {
        branch_var: var_store.fresh(),
        cond_var: var_store.fresh(),
        branches: vec![(
            // if condition
            no_region(
                // Int.neq arg1 0
                RunLowLevel {
                    op: LowLevel::NotEq,
                    args: vec![
                        (var_store.fresh(), Var(Symbol::INT_REM_ARG_1)),
                        (var_store.fresh(), Int(var_store.fresh(), 0)),
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
                        // Int.#remUnsafe arg0 arg1
                        call(
                            Symbol::INT_REM_UNSAFE,
                            vec![Var(Symbol::INT_REM_ARG_0), Var(Symbol::INT_REM_ARG_1)],
                            var_store,
                        ),
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
        vec![Symbol::INT_REM_ARG_0, Symbol::INT_REM_ARG_1],
        var_store,
        body,
    )
}

/// Int.abs : Int -> Int
fn int_abs(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    let body = If {
        branch_var: var_store.fresh(),
        cond_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // Int.isLt 0 n
                // 0 < n
                call(
                    Symbol::INT_LT,
                    vec![Int(var_store.fresh(), 0), Var(Symbol::INT_ABS_ARG)],
                    var_store,
                ),
            ),
            // int is at least 0, so just pass it along
            no_region(Var(Symbol::INT_ABS_ARG)),
        )],
        final_else: Box::new(
            // int is below 0, so negate it.
            no_region(call(
                Symbol::NUM_NEG,
                vec![Var(Symbol::INT_ABS_ARG)],
                var_store,
            )),
        ),
    };

    defn(symbol, vec![Symbol::INT_ABS_ARG], var_store, body)
}

/// Int.div : Int, Int -> Result Int [ DivByZero ]*
fn int_div(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

    let body = If {
        branch_var: var_store.fresh(),
        cond_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            no_region(
                // Int.neq denominator 0
                RunLowLevel {
                    op: LowLevel::NotEq,
                    args: vec![
                        (var_store.fresh(), Var(Symbol::INT_DIV_ARG_DENOMINATOR)),
                        (var_store.fresh(), Int(var_store.fresh(), 0)),
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
                        // Int.#divUnsafe numerator denominator
                        call(
                            Symbol::INT_DIV_UNSAFE,
                            vec![
                                Var(Symbol::INT_DIV_ARG_NUMERATOR),
                                Var(Symbol::INT_DIV_ARG_DENOMINATOR),
                            ],
                            var_store,
                        ),
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
        vec![
            Symbol::INT_DIV_ARG_NUMERATOR,
            Symbol::INT_DIV_ARG_DENOMINATOR,
        ],
        var_store,
        body,
    )
}

/// List.first : List elem -> Result elem [ ListWasEmpty ]*
fn list_first(symbol: Symbol, var_store: &mut VarStore) -> Def {
    use crate::expr::Expr::*;

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
                call(
                    Symbol::LIST_IS_EMPTY,
                    vec![Var(Symbol::LIST_FIRST_ARG)],
                    var_store,
                ),
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
                                (var_store.fresh(), Var(Symbol::LIST_GET_ARG_LIST)),
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

    defn(symbol, vec![Symbol::LIST_FIRST_ARG], var_store, body)
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
fn call(symbol: Symbol, args: Vec<Expr>, var_store: &mut VarStore) -> Expr {
    Expr::Call(
        Box::new((
            var_store.fresh(),
            no_region(Expr::Var(symbol)),
            var_store.fresh(),
        )),
        args.into_iter()
            .map(|expr| (var_store.fresh(), no_region(expr)))
            .collect::<Vec<(Variable, Located<Expr>)>>(),
        CalledVia::Space,
    )
}

#[inline(always)]
fn defn(fn_name: Symbol, args: Vec<Symbol>, var_store: &mut VarStore, body: Expr) -> Def {
    use crate::expr::Expr::*;
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
