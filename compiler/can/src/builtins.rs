use crate::def::Def;
use crate::expr::{Expr, Recursive};
use roc_collections::all::{MutMap, SendMap};
use roc_module::ident::TagName;
use roc_module::operator::CalledVia;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::subs::{VarStore, Variable};

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
pub fn builtin_defs(var_store: &VarStore) -> MutMap<Symbol, Def> {
    mut_map! {
        Symbol::LIST_GET => list_get(var_store),
        Symbol::LIST_FIRST => list_first(var_store),
        Symbol::INT_DIV => int_div(var_store),
        Symbol::INT_ABS => int_abs(var_store),
        Symbol::INT_REM => int_rem(var_store),
        Symbol::INT_IS_ODD => int_is_odd(var_store),
        Symbol::INT_IS_EVEN => int_is_even(var_store),
        Symbol::INT_IS_ZERO => int_is_zero(var_store),
        Symbol::INT_IS_POSITIVE => int_is_positive(var_store),
        Symbol::INT_IS_NEGATIVE => int_is_negative(var_store),
        Symbol::FLOAT_IS_POSITIVE => float_is_positive(var_store),
        Symbol::FLOAT_IS_NEGATIVE => float_is_negative(var_store),
        Symbol::FLOAT_IS_ZERO => float_is_zero(var_store),
    }
}

/// Float.isZero : Float -> Bool
fn float_is_zero(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;

    defn(
        Symbol::FLOAT_IS_ZERO,
        vec![Symbol::FLOAT_IS_ZERO_ARG],
        var_store,
        call(
            Symbol::FLOAT_EQ,
            vec![
                Float(var_store.fresh(), 0.0),
                Var(Symbol::FLOAT_IS_ZERO_ARG),
            ],
            var_store,
        ),
    )
}

/// Float.isNegative : Float -> Bool
fn float_is_negative(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;

    defn(
        Symbol::FLOAT_IS_NEGATIVE,
        vec![Symbol::FLOAT_IS_NEGATIVE_ARG],
        var_store,
        call(
            Symbol::FLOAT_GT,
            vec![
                Float(var_store.fresh(), 0.0),
                Var(Symbol::FLOAT_IS_NEGATIVE_ARG),
            ],
            var_store,
        ),
    )
}

/// Float.isPositive : Float -> Bool
fn float_is_positive(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;

    defn(
        Symbol::FLOAT_IS_POSITIVE,
        vec![Symbol::FLOAT_IS_POSITIVE_ARG],
        var_store,
        call(
            Symbol::FLOAT_GT,
            vec![
                Var(Symbol::FLOAT_IS_POSITIVE_ARG),
                Float(var_store.fresh(), 0.0),
            ],
            var_store,
        ),
    )
}

/// Int.isNegative : Int -> Bool
fn int_is_negative(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;

    defn(
        Symbol::INT_IS_NEGATIVE,
        vec![Symbol::INT_IS_NEGATIVE_ARG],
        var_store,
        call(
            Symbol::NUM_LT,
            vec![Var(Symbol::INT_IS_NEGATIVE_ARG), Int(var_store.fresh(), 0)],
            var_store,
        ),
    )
}

/// Int.isPositive : Int -> Bool
fn int_is_positive(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;

    defn(
        Symbol::INT_IS_POSITIVE,
        vec![Symbol::INT_IS_POSITIVE_ARG],
        var_store,
        call(
            Symbol::NUM_GT,
            vec![Var(Symbol::INT_IS_POSITIVE_ARG), Int(var_store.fresh(), 0)],
            var_store,
        ),
    )
}

/// Int.isZero : Int -> Bool
fn int_is_zero(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;

    defn(
        Symbol::INT_IS_ZERO,
        vec![Symbol::INT_IS_ZERO_ARG],
        var_store,
        call(
            Symbol::INT_EQ_I64,
            vec![Var(Symbol::INT_IS_ZERO_ARG), Int(var_store.fresh(), 0)],
            var_store,
        ),
    )
}

/// Int.isOdd : Int -> Bool
fn int_is_odd(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;

    defn(
        Symbol::INT_IS_ODD,
        vec![Symbol::INT_IS_ODD_ARG],
        var_store,
        call(
            Symbol::INT_EQ_I64,
            vec![
                call(
                    Symbol::INT_REM_UNSAFE,
                    vec![Var(Symbol::INT_IS_ODD_ARG), Int(var_store.fresh(), 2)],
                    var_store,
                ),
                Int(var_store.fresh(), 1),
            ],
            var_store,
        ),
    )
}

/// Int.isEven : Int -> Bool
fn int_is_even(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;

    defn(
        Symbol::INT_IS_EVEN,
        vec![Symbol::INT_IS_EVEN_ARG],
        var_store,
        call(
            Symbol::INT_EQ_I64,
            vec![
                call(
                    Symbol::INT_REM_UNSAFE,
                    vec![Var(Symbol::INT_IS_EVEN_ARG), Int(var_store.fresh(), 2)],
                    var_store,
                ),
                Int(var_store.fresh(), 0),
            ],
            var_store,
        ),
    )
}

/// List.get : List elem, Int -> Result elem [ OutOfBounds ]*
fn list_get(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;

    defn(
        Symbol::LIST_GET,
        vec![Symbol::LIST_GET_ARG_LIST, Symbol::LIST_GET_ARG_INDEX],
        var_store,
        // Perform a bounds check. If it passes, delegate to List.#getUnsafe
        If {
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
                            // List.getUnsafe list index
                            Call(
                                Box::new((
                                    var_store.fresh(),
                                    no_region(Var(Symbol::LIST_GET_UNSAFE)),
                                    var_store.fresh(),
                                )),
                                vec![
                                    (var_store.fresh(), no_region(Var(Symbol::LIST_GET_ARG_LIST))),
                                    (
                                        var_store.fresh(),
                                        no_region(Var(Symbol::LIST_GET_ARG_INDEX)),
                                    ),
                                ],
                                CalledVia::Space,
                            ),
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
        },
    )
}

/// Int.rem : Int, Int -> Int
fn int_rem(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;

    defn(
        Symbol::INT_REM,
        vec![Symbol::INT_REM_ARG_0, Symbol::INT_REM_ARG_1],
        var_store,
        If {
            branch_var: var_store.fresh(),
            cond_var: var_store.fresh(),
            branches: vec![(
                // if condition
                no_region(
                    // Int.neq arg1 0
                    call(
                        Symbol::INT_NEQ_I64,
                        vec![Var(Symbol::INT_REM_ARG_1), (Int(var_store.fresh(), 0))],
                        var_store,
                    ),
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
        },
    )
}

/// Int.abs : Int -> Int
fn int_abs(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;

    defn(
        Symbol::INT_ABS,
        vec![Symbol::INT_ABS_ARG],
        var_store,
        If {
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
        },
    )
}

/// Int.div : Int, Int -> Result Int [ DivByZero ]*
fn int_div(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;

    defn(
        Symbol::INT_DIV,
        vec![
            Symbol::INT_DIV_ARG_NUMERATOR,
            Symbol::INT_DIV_ARG_DENOMINATOR,
        ],
        var_store,
        If {
            branch_var: var_store.fresh(),
            cond_var: var_store.fresh(),
            branches: vec![(
                // if-condition
                no_region(
                    // Int.neq denominator 0
                    call(
                        Symbol::INT_NEQ_I64,
                        vec![
                            Var(Symbol::INT_DIV_ARG_DENOMINATOR),
                            (Int(var_store.fresh(), 0)),
                        ],
                        var_store,
                    ),
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
        },
    )
}

/// List.first : List elem -> Result elem [ ListWasEmpty ]*
fn list_first(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;

    defn(
        Symbol::LIST_FIRST,
        vec![Symbol::LIST_FIRST_ARG],
        var_store,
        // Perform a bounds check. If it passes, delegate to List.getUnsafe.
        If {
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
                            // List.#getUnsafe list 0
                            call(
                                Symbol::LIST_GET_UNSAFE,
                                vec![(Var(Symbol::LIST_FIRST_ARG)), (Int(var_store.fresh(), 0))],
                                var_store,
                            ),
                        ],
                        var_store,
                    ),
                ),
            ),
        },
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
fn tag(name: &'static str, args: Vec<Expr>, var_store: &VarStore) -> Expr {
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
fn call(symbol: Symbol, args: Vec<Expr>, var_store: &VarStore) -> Expr {
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
fn defn(fn_name: Symbol, args: Vec<Symbol>, var_store: &VarStore, body: Expr) -> Def {
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
        loc_pattern: no_region(Identifier(fn_name)),
        loc_expr: no_region(expr),
        expr_var: var_store.fresh(),
        pattern_vars: SendMap::default(),
        annotation: None,
    }
}
