use roc_can::constraint::Constraint;
use roc_can::def::Def;
use roc_can::expected::Expected;
use roc_can::expr::Expr;
use roc_can::pattern::symbols_from_pattern;
use roc_collections::all::MutSet;
use roc_constrain::expr::exists;
use roc_module::symbol::Symbol;
use roc_region::all::Region;
use roc_solve::solve;
use roc_types::subs::{Subs, VarStore};
use roc_types::types::{Category, Type};

pub fn infer_closure_size(def: &Def, subs: &mut Subs, solve_env: &solve::Env) {
    infer_closure_size_expr(&def.loc_expr.value, subs, solve_env);
}

/// NOTE this is only safe to run on a top-level definition
fn infer_closure_size_expr(expr: &Expr, subs: &mut Subs, solve_env: &solve::Env) {
    let mut var_store = VarStore::new_from_subs(subs);

    let mut problems = Vec::new();
    let constraint = generate_constraint(expr, &mut var_store);

    let next = var_store.fresh().index();
    let variables_introduced = next as usize - (subs.len() - 1);

    subs.extend_by(variables_introduced);

    let _new_env = solve::run_in_place(solve_env, &mut problems, subs, &constraint);

    debug_assert_eq!(problems.len(), 0);
}

pub fn free_variables(expr: &Expr) -> MutSet<Symbol> {
    use Expr::*;

    let mut stack = vec![expr.clone()];

    let mut bound = MutSet::default();
    let mut used: std::collections::HashSet<roc_module::symbol::Symbol, _> = MutSet::default();

    while let Some(expr) = stack.pop() {
        match expr {
            Num(_, _) | Int(_, _) | Float(_, _) | Str(_) | EmptyRecord | RuntimeError(_) => {}

            Var(s) => {
                used.insert(s);
            }
            List { loc_elems, .. } => {
                for e in loc_elems {
                    stack.push(e.value);
                }
            }
            When {
                loc_cond, branches, ..
            } => {
                stack.push(loc_cond.value);

                for branch in branches {
                    stack.push(branch.value.value);

                    if let Some(guard) = branch.guard {
                        stack.push(guard.value);
                    }

                    bound.extend(
                        branch
                            .patterns
                            .iter()
                            .map(|t| symbols_from_pattern(&t.value).into_iter())
                            .flatten(),
                    );
                }
            }
            If {
                branches,
                final_else,
                ..
            } => {
                for (cond, then) in branches {
                    stack.push(cond.value);
                    stack.push(then.value);
                }
                stack.push(final_else.value);
            }

            LetRec(defs, cont, _, _) => {
                stack.push(cont.value);

                for def in defs {
                    bound.extend(symbols_from_pattern(&def.loc_pattern.value));
                    stack.push(def.loc_expr.value);
                }
            }
            LetNonRec(def, cont, _, _) => {
                stack.push(cont.value);

                bound.extend(symbols_from_pattern(&def.loc_pattern.value));
                stack.push(def.loc_expr.value);
            }
            Call(boxed, args, _) => {
                let (_, function, _, _) = *boxed;

                stack.push(function.value);
                for (_, arg) in args {
                    stack.push(arg.value);
                }
            }
            RunLowLevel { args, .. } => {
                for (_, arg) in args {
                    stack.push(arg);
                }
            }

            Closure {
                arguments: args,
                loc_body: boxed_body,
                ..
            } => {
                bound.extend(
                    args.iter()
                        .map(|t| symbols_from_pattern(&t.1.value).into_iter())
                        .flatten(),
                );
                stack.push(boxed_body.value);
            }
            Record { fields, .. } => {
                for (_, field) in fields {
                    stack.push(field.loc_expr.value);
                }
            }
            Update {
                symbol, updates, ..
            } => {
                used.insert(symbol);
                for (_, field) in updates {
                    stack.push(field.loc_expr.value);
                }
            }
            Access { loc_expr, .. } => {
                stack.push(loc_expr.value);
            }

            Accessor { .. } => {}

            Tag { arguments, .. } => {
                for (_, arg) in arguments {
                    stack.push(arg.value);
                }
            }
        }
    }

    for b in bound {
        used.remove(&b);
    }

    used
}

fn generate_constraint(expr: &Expr, var_store: &mut VarStore) -> Constraint {
    let mut constraints = Vec::new();
    generate_constraints_help(expr, var_store, &mut constraints);
    Constraint::And(constraints)
}

pub fn generate_constraints_help(
    expr: &Expr,
    var_store: &mut VarStore,
    constraints: &mut Vec<Constraint>,
) {
    use Expr::*;

    match expr {
        Num(_, _) | Int(_, _) | Float(_, _) | Str(_) | EmptyRecord | RuntimeError(_) => {}

        Var(_) => {}
        List { loc_elems, .. } => {
            for e in loc_elems {
                generate_constraints_help(&e.value, var_store, constraints);
            }
        }
        When {
            loc_cond, branches, ..
        } => {
            generate_constraints_help(&loc_cond.value, var_store, constraints);

            for branch in branches {
                generate_constraints_help(&branch.value.value, var_store, constraints);

                if let Some(guard) = &branch.guard {
                    generate_constraints_help(&guard.value, var_store, constraints);
                }
            }
        }
        If {
            branches,
            final_else,
            ..
        } => {
            for (cond, then) in branches {
                generate_constraints_help(&cond.value, var_store, constraints);
                generate_constraints_help(&then.value, var_store, constraints);
            }
            generate_constraints_help(&final_else.value, var_store, constraints);
        }

        LetRec(defs, cont, _, _) => {
            generate_constraints_help(&cont.value, var_store, constraints);

            for def in defs {
                generate_constraints_help(&def.loc_expr.value, var_store, constraints);
            }
        }
        LetNonRec(def, cont, _, _) => {
            generate_constraints_help(&cont.value, var_store, constraints);

            generate_constraints_help(&def.loc_expr.value, var_store, constraints);
        }
        Call(boxed, args, _) => {
            let (_, function, _, _) = &**boxed;

            generate_constraints_help(&function.value, var_store, constraints);
            for (_, arg) in args {
                generate_constraints_help(&arg.value, var_store, constraints);
            }
        }
        RunLowLevel { args, .. } => {
            for (_, arg) in args {
                generate_constraints_help(&arg, var_store, constraints);
            }
        }

        Closure {
            closure_type: closure_var,
            loc_body: boxed_body,
            captured_symbols,
            ..
        } => {
            let mut cons = Vec::new();
            let mut variables = Vec::new();

            let closed_over_symbols = captured_symbols;

            let closure_ext_var = var_store.fresh();
            let closure_var = *closure_var;

            variables.push(closure_ext_var);
            // TODO unsure about including this one
            variables.push(closure_var);

            let mut tag_arguments = Vec::with_capacity(closed_over_symbols.len());
            for symbol in closed_over_symbols {
                let var = var_store.fresh();
                variables.push(var);
                tag_arguments.push(Type::Variable(var));

                let region = Region::zero();
                let expected = Expected::NoExpectation(Type::Variable(var));
                let lookup = Constraint::Lookup(*symbol, expected, region);
                cons.push(lookup);
            }

            let tag_name_string = format!("Closure_{}", closure_var.index());
            let tag_name = roc_module::ident::TagName::Global(tag_name_string.into());
            let expected_type = Type::TagUnion(
                vec![(tag_name, tag_arguments)],
                Box::new(Type::Variable(closure_ext_var)),
            );

            // constrain this closures's size to the type we just created
            let expected = Expected::NoExpectation(expected_type);
            let category = Category::ClosureSize;
            let region = boxed_body.region;
            let equality = Constraint::Eq(Type::Variable(closure_var), expected, category, region);

            cons.push(equality);

            // generate constraints for nested closures
            let mut inner_constraints = Vec::new();
            generate_constraints_help(&boxed_body.value, var_store, &mut inner_constraints);

            cons.push(Constraint::And(inner_constraints));

            let constraint = exists(variables, Constraint::And(cons));

            constraints.push(constraint);
        }
        Record { fields, .. } => {
            for (_, field) in fields {
                generate_constraints_help(&field.loc_expr.value, var_store, constraints);
            }
        }
        Update {
            symbol: _, updates, ..
        } => {
            for (_, field) in updates {
                generate_constraints_help(&field.loc_expr.value, var_store, constraints);
            }
        }
        Access { loc_expr, .. } => {
            generate_constraints_help(&loc_expr.value, var_store, constraints);
        }

        Accessor { .. } => {}

        Tag { arguments, .. } => {
            for (_, arg) in arguments {
                generate_constraints_help(&arg.value, var_store, constraints);
            }
        }
    }
}
