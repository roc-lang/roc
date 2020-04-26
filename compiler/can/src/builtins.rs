use crate::def::Def;
use crate::expr::Expr;
use crate::expr::Recursive;
use roc_collections::all::SendMap;
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_parse::operator::CalledVia;
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
pub fn builtin_defs(var_store: &VarStore) -> Vec<Def> {
    vec![list_get(var_store), list_first(var_store)]
}

/// List.get : List elem, Int -> Result elem [ OutOfBounds ]*
fn list_get(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;
    use crate::pattern::Pattern::*;

    let args = vec![
        (
            var_store.fresh(),
            no_region(Identifier(Symbol::LIST_GET_ARG_LIST)),
        ),
        (
            var_store.fresh(),
            no_region(Identifier(Symbol::LIST_GET_ARG_INDEX)),
        ),
    ];

    // Perform a bounds check. If it passes, delegate to List.#getUnsafe
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
    };

    let expr = Closure(
        var_store.fresh(),
        Symbol::LIST_GET,
        Recursive::NotRecursive,
        args,
        Box::new((no_region(body), var_store.fresh())),
    );

    Def {
        loc_pattern: no_region(Identifier(Symbol::LIST_GET)),
        loc_expr: no_region(expr),
        expr_var: var_store.fresh(),
        pattern_vars: SendMap::default(),
        annotation: None,
    }
}

/// List.first : List elem -> Result elem [ ListWasEmpty ]*
fn list_first(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;
    use crate::pattern::Pattern::*;

    let args = vec![(
        var_store.fresh(),
        no_region(Identifier(Symbol::LIST_FIRST_ARG)),
    )];

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
    };

    let expr = Closure(
        var_store.fresh(),
        Symbol::LIST_FIRST,
        Recursive::NotRecursive,
        args,
        Box::new((no_region(body), var_store.fresh())),
    );

    Def {
        loc_pattern: no_region(Identifier(Symbol::LIST_FIRST)),
        loc_expr: no_region(expr),
        expr_var: var_store.fresh(),
        pattern_vars: SendMap::default(),
        annotation: None,
    }
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
