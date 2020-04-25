use crate::def::Def;
use crate::expr::Recursive;
use roc_collections::all::SendMap;
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_parse::operator::CalledVia;
use roc_region::all::{Located, Region};
use roc_types::subs::VarStore;

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

    // Perform a bounds check. If it passes, delegate to List.getUnsafe
    let body = If {
        cond_var: var_store.fresh(),
        branch_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            Located {
                region: Region::zero(),
                value: Call(
                    Box::new((
                        var_store.fresh(),
                        Located {
                            region: Region::zero(),
                            value: Var(Symbol::LIST_IS_EMPTY), // TODO actually check if (index < List.len list)
                        },
                        var_store.fresh(),
                    )),
                    vec![(
                        var_store.fresh(),
                        Located {
                            region: Region::zero(),
                            value: Var(Symbol::LIST_GET_ARG_LIST),
                        },
                    )],
                    CalledVia::Space,
                ),
            },
            // then-branch
            Located {
                region: Region::zero(),
                value:
                // Ok
                Tag {
                    variant_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    name: TagName::Global("Ok".into()),
                    arguments: vec![(
                        var_store.fresh(),
                        no_region(
                            // List.getUnsafe list index
                            Call(
                                Box::new((var_store.fresh(), no_region(Var(Symbol::LIST_GET_UNSAFE)), var_store.fresh())),
                                vec![
                                    (var_store.fresh(), no_region(Var(Symbol::LIST_GET_ARG_LIST))),
                                    (var_store.fresh(), no_region(Var(Symbol::LIST_GET_ARG_INDEX))),
                                ],
                                CalledVia::Space,
                            )
                        )
                    )]
                },
            },
        )],
        final_else: Box::new(
            // else-branch
            no_region(
                // Err
                Tag {
                    variant_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    name: TagName::Global("Err".into()),
                    arguments: vec![(
                        var_store.fresh(),
                        no_region(Tag {
                            variant_var: var_store.fresh(),
                            ext_var: var_store.fresh(),
                            name: TagName::Global("OutOfBounds".into()),
                            arguments: std::vec::Vec::new(),
                        }),
                    )],
                },
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

/// List.first : List elem -> Result elem [ OutOfBounds ]*
fn list_first(var_store: &VarStore) -> Def {
    use crate::expr::Expr::*;
    use crate::pattern::Pattern::*;

    let args = vec![(
        var_store.fresh(),
        no_region(Identifier(Symbol::LIST_FIRST_ARG)),
    )];

    // Perform a bounds check. If it passes, delegate to List.getUnsafe.
    let body =
        // Use "when" instead of "if" so that we can have False be the first branch.
        // We want that for branch prediction; usually we expect the list to be nonempty.
        If {
        cond_var: var_store.fresh(),
        branch_var: var_store.fresh(),
        branches: vec![(
            // if-condition
            Located {
                region: Region::zero(),
                value: Call(
                    Box::new((
                        var_store.fresh(),
                        Located {
                            region: Region::zero(),
                            value: Var(Symbol::LIST_IS_EMPTY),
                        },
                        var_store.fresh(),
                    )),
                    vec![(
                        var_store.fresh(),
                        Located {
                            region: Region::zero(),
                            value: Var(Symbol::LIST_GET_ARG_LIST),
                        },
                    )],
                    CalledVia::Space,
                ),
            },
            // then-branch
            Located {
                region: Region::zero(),
                value:
                // Ok
                Tag {
                    variant_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    name: TagName::Global("Ok".into()),
                    arguments: vec![(
                        var_store.fresh(),
                        no_region(
                            // List.getUnsafe list index
                            Call(
                                Box::new((var_store.fresh(), no_region(Var(Symbol::LIST_GET_UNSAFE)), var_store.fresh())),
                                vec![
                                    (var_store.fresh(), no_region(Var(Symbol::LIST_GET_ARG_LIST))),
                                    (var_store.fresh(), no_region(Var(Symbol::LIST_GET_ARG_INDEX))),
                                ],
                                CalledVia::Space,
                            )
                        )
                    )]
                },
            },
        )],
        final_else: Box::new(
            // default branch
            no_region(
                // Err
                Tag {
                    variant_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    name: TagName::Global("Err".into()),
                    arguments: vec![(
                        var_store.fresh(),
                        no_region(Tag {
                            variant_var: var_store.fresh(),
                            ext_var: var_store.fresh(),
                            name: TagName::Global("OutOfBounds".into()),
                            arguments: std::vec::Vec::new(),
                        }),
                    )],
                },
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

#[inline(always)]
fn no_region<T>(value: T) -> Located<T> {
    Located {
        region: Region::zero(),
        value,
    }
}
