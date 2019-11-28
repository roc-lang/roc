use crate::can::symbol::Symbol;
use crate::collections::ImMap;
use crate::subs::{Content, Descriptor, FlatType, Subs, Variable};
use crate::types::Constraint::{self, *};
use crate::types::Type::{self, *};

type Env = ImMap<Symbol, Variable>;

pub fn solve(env: &Env, subs: &mut Subs, constraint: &Constraint) {
    match constraint {
        True => (),
        Eq(typ, expected_type, _region) => {
            // TODO use region?
            let actual = type_to_variable(subs, typ.clone());
            let expected = type_to_variable(subs, expected_type.clone().get_type());

            subs.union(actual, expected);
        }
        Lookup(symbol, expected_type, _region) => {
            // TODO use region?
            let actual =
                subs.copy_var(*env.get(&symbol).unwrap_or_else(|| {
                    panic!("Could not find symbol {:?} in env {:?}", symbol, env)
                }));
            let expected = type_to_variable(subs, expected_type.clone().get_type());

            subs.union(actual, expected);
        }
        And(sub_constraints) => {
            for sub_constraint in sub_constraints.iter() {
                solve(env, subs, sub_constraint);
            }
        }
        Pattern(_region, _category, typ, expected) => {
            // TODO use region?
            let actual = type_to_variable(subs, typ.clone());
            let expected = type_to_variable(subs, expected.clone().get_type());

            subs.union(actual, expected);
        }
        Let(let_con) => {
            match &let_con.ret_constraint {
                True => {
                    // If the return expression is guaranteed to solve,
                    // solve the assignments themselves and move on.
                    solve(env, subs, &let_con.defs_constraint)
                }
                ret_con => {
                    // Solve the assignments' constraints first.
                    solve(env, subs, &let_con.defs_constraint);

                    // Add a variable for each assignment to the env.
                    let mut new_env = env.clone();

                    for (symbol, loc_type) in let_con.def_types.iter() {
                        // We must not overwrite existing symbols! If we do,
                        // we will overwrite procedure entries, which were
                        // inserted earlier in solving. (If we allowed
                        // shadowing, we'd need to do something fancier here.)
                        if !new_env.contains_key(&symbol) {
                            let var = type_to_variable(subs, loc_type.value.clone());

                            new_env.insert(symbol.clone(), var);
                        }
                    }

                    // Now solve the body, using the new env which includes
                    // the assignments' name-to-variable mappings.
                    solve(&new_env, subs, &ret_con);

                    // TODO do an occurs check for each of the assignments!
                }
            }
        }
    }
}

fn type_to_variable(subs: &mut Subs, typ: Type) -> Variable {
    match typ {
        Variable(var) => var,
        Apply {
            module_name,
            name,
            args,
        } => {
            let mut arg_vars = Vec::with_capacity(args.len());

            for arg in args {
                arg_vars.push(type_to_variable(subs, arg.clone()))
            }

            let flat_type = FlatType::Apply {
                module_name,
                name,
                args: arg_vars,
            };
            let content = Content::Structure(flat_type);

            subs.fresh(Descriptor::from(content))
        }
        EmptyRec => {
            let content = Content::Structure(FlatType::EmptyRecord);

            subs.fresh(Descriptor::from(content))
        }
        Function(args, ret_type) => {
            let mut arg_vars = Vec::with_capacity(args.len());

            for arg in args {
                arg_vars.push(type_to_variable(subs, arg.clone()))
            }

            let ret_var = type_to_variable(subs, *ret_type);
            let content: Content = Content::Structure(FlatType::Func(arg_vars, ret_var));

            subs.fresh(Descriptor::from(content))
        }
        Erroneous(problem) => {
            let content = Content::Structure(FlatType::Erroneous(problem));

            subs.fresh(Descriptor::from(content))
        }
    }
}
