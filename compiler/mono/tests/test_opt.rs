#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc_mono;

mod helpers;

// Test optimizations
#[cfg(test)]
mod test_opt {
    use crate::helpers::{infer_expr, uniq_expr};
    use bumpalo::Bump;
    use roc_module::low_level::LowLevel;
    use roc_module::symbol::Symbol;
    use roc_mono::expr::Expr::{self, *};
    use roc_mono::expr::Procs;
    use roc_mono::layout::{Builtin, Layout};

    // HELPERS

    #[derive(Debug, Default, PartialEq, Eq)]
    struct CallProblems {
        missing: Vec<Symbol>,
        unexpected: Vec<Symbol>,
    }

    fn contains_named_calls(src: &str, mut calls: Vec<Symbol>) {
        let arena = Bump::new();
        let (loc_expr, _, _problems, subs, var, constraint, home, mut interns) = uniq_expr(src);

        let mut unify_problems = Vec::new();
        let (_content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        // Compile and add all the Procs before adding main
        let mut procs = Procs::default();
        let mut ident_ids = interns.all_ident_ids.remove(&home).unwrap();

        // assume 64-bit pointers
        let pointer_size = std::mem::size_of::<u64>() as u32;

        // Populate Procs and Subs, and get the low-level Expr from the canonical Expr
        let mut mono_problems = Vec::new();
        let mut mono_env = roc_mono::expr::Env {
            arena: &arena,
            subs: &mut subs,
            problems: &mut mono_problems,
            home,
            ident_ids: &mut ident_ids,
            pointer_size,
            jump_counter: arena.alloc(0),
        };
        let mono_expr = Expr::new(&mut mono_env, loc_expr.value, &mut procs);

        let unexpected_calls = extract_named_calls(&mono_expr, &mut calls);
        let expected = CallProblems::default();
        let actual = CallProblems {
            missing: calls,
            unexpected: unexpected_calls,
        };

        assert_eq!(expected, actual);
    }

    fn extract_named_calls(expr: &Expr<'_>, calls: &mut Vec<Symbol>) -> Vec<Symbol> {
        let mut unexpected_calls = Vec::new();

        // The calls must be sorted so we can binary_search them for matches.
        calls.sort();

        extract_named_calls_help(expr, calls, &mut unexpected_calls);

        unexpected_calls
    }
    fn extract_named_calls_help(
        expr: &Expr<'_>,
        calls: &mut Vec<Symbol>,
        unexpected_calls: &mut Vec<Symbol>,
    ) {
        match expr {
            Int(_)
            | Float(_)
            | Str(_)
            | Bool(_)
            | Byte(_)
            | Load(_)
            | FunctionPointer(_, _)
            | RunLowLevel(_, _)
            | RuntimeError(_)
            | RuntimeErrorFunction(_) => (),

            Store(paths, sub_expr) => {
                for (_, _, path_expr) in paths.iter() {
                    extract_named_calls_help(path_expr, calls, unexpected_calls);
                }

                extract_named_calls_help(sub_expr, calls, unexpected_calls);
            }

            CallByPointer(sub_expr, args, _) => {
                extract_named_calls_help(sub_expr, calls, unexpected_calls);

                for arg in args.iter() {
                    extract_named_calls_help(arg, calls, unexpected_calls);
                }
            }

            CallByName {
                name,
                layout: _,
                args,
            } => {
                // Search for the symbol. If we found it, check it off the list.
                // If we didn't find it, add it to the list of unexpected calls.
                match calls.binary_search(name) {
                    Ok(index) => {
                        calls.remove(index);
                    }
                    Err(_) => {
                        unexpected_calls.push(*name);
                    }
                }

                for (arg, _) in args.iter() {
                    extract_named_calls_help(arg, calls, unexpected_calls);
                }
            }

            Cond {
                cond_symbol: _,
                branch_symbol: _,
                cond_layout: _,
                pass,
                fail,
                ret_layout: _,
            } => {
                extract_named_calls_help(pass.1, calls, unexpected_calls);
                extract_named_calls_help(fail.1, calls, unexpected_calls);
            }
            Switch {
                cond,
                cond_layout: _,
                branches,
                default_branch,
                ret_layout: _,
            } => {
                extract_named_calls_help(cond, calls, unexpected_calls);
                extract_named_calls_help(default_branch.1, calls, unexpected_calls);

                for (_, _, branch_expr) in branches.iter() {
                    extract_named_calls_help(branch_expr, calls, unexpected_calls);
                }
            }

            Tag {
                tag_layout: _,
                tag_name: _,
                tag_id: _,
                union_size: _,
                arguments,
            } => {
                for (tag_expr, _) in arguments.iter() {
                    extract_named_calls_help(tag_expr, calls, unexpected_calls);
                }
            }
            Struct(fields) => {
                for (field, _) in fields.iter() {
                    extract_named_calls_help(field, calls, unexpected_calls);
                }
            }
            AccessAtIndex {
                index: _,
                field_layouts: _,
                expr: sub_expr,
                is_unwrapped: _,
            } => {
                extract_named_calls_help(sub_expr, calls, unexpected_calls);
            }

            Array {
                elem_layout: _,
                elems,
            } => {
                for elem in elems.iter() {
                    extract_named_calls_help(elem, calls, unexpected_calls);
                }
            }
        }
    }

    fn compiles_to(src: &str, expected: Expr<'_>) {
        let arena = Bump::new();
        let (loc_expr, _, _problems, subs, var, constraint, home, mut interns) = uniq_expr(src);

        let mut unify_problems = Vec::new();
        let (_content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        // Compile and add all the Procs before adding main
        let mut procs = Procs::default();
        let mut ident_ids = interns.all_ident_ids.remove(&home).unwrap();

        // assume 64-bit pointers
        let pointer_size = std::mem::size_of::<u64>() as u32;

        // Populate Procs and Subs, and get the low-level Expr from the canonical Expr
        let mut mono_problems = Vec::new();
        let mut mono_env = roc_mono::expr::Env {
            arena: &arena,
            subs: &mut subs,
            problems: &mut mono_problems,
            home,
            ident_ids: &mut ident_ids,
            pointer_size,
            jump_counter: arena.alloc(0),
        };
        let mono_expr = Expr::new(&mut mono_env, loc_expr.value, &mut procs);

        assert_eq!(mono_expr, expected);
    }

    #[test]
    fn int_literal() {
        compiles_to("5", Int(5));
    }

    #[test]
    fn float_literal() {
        compiles_to("0.5", Float(0.5));
    }

    #[test]
    fn set_unique_int_list() {
        // This should optimize List.set to List.set_in_place
        compiles_to(
            "List.getUnsafe (List.set [ 12, 9, 7, 3 ] 1 42) 1",
            RunLowLevel(
                LowLevel::ListGetUnsafe,
                &vec![
                    (
                        CallByName {
                            name: Symbol::LIST_SET_IN_PLACE,
                            layout: Layout::FunctionPointer(
                                &[
                                    Layout::Builtin(Builtin::List(&Layout::Builtin(
                                        Builtin::Int64,
                                    ))),
                                    Layout::Builtin(Builtin::Int64),
                                    Layout::Builtin(Builtin::Int64),
                                ],
                                &Layout::Builtin(Builtin::List(&Layout::Builtin(Builtin::Int64))),
                            ),
                            args: &vec![
                                (
                                    Array {
                                        elem_layout: Layout::Builtin(Builtin::Int64),
                                        elems: &vec![Int(12), Int(9), Int(7), Int(3)],
                                    },
                                    Layout::Builtin(Builtin::List(&Layout::Builtin(
                                        Builtin::Int64,
                                    ))),
                                ),
                                (Int(1), Layout::Builtin(Builtin::Int64)),
                                (Int(42), Layout::Builtin(Builtin::Int64)),
                            ],
                        },
                        Layout::Builtin(Builtin::List(&Layout::Builtin(Builtin::Int64))),
                    ),
                    (Int(1), Layout::Builtin(Builtin::Int64)),
                ],
            ),
        );
    }

    #[test]
    fn set_shared_int_list() {
        // This should *NOT* optimize List.set to List.set_in_place
        contains_named_calls(
            indoc!(
                r#"
                    shared = [ 2, 4 ]

                    # This should not mutate the original
                    x = List.set shared 1 0

                    { x, y: List.getUnsafe shared 1 }
                "#
            ),
            vec![
                Symbol::LIST_SET, /* Symbol::LIST_GET_UNSAFE TODO revise this test */
            ],
        );
    }
}
