// type Env =
//   Map.Map Name.Name Variable


// type Pools =
//   MVector.IOVector [Variable]


// data State =
//   State
//     { _env :: Env
//     , _mark :: Mark
//     , _errors :: [Error.Error]
//     }

use subs::{Subs, Variable, Descriptor, Content, FlatType};
use collections::ImMap;
use canonicalize::Symbol;
use types::Constraint::{self, *};
use types::Type::{self, *};

type Env = ImMap<Symbol, Variable>;

pub fn solve(env: &Env, subs: &mut Subs, constraint: Constraint) {
    // println!("\nSolving:\n\n\t{:?}\n\n", constraint);
    match constraint {
        True => (),
        Eq(typ, expected_type, region) => {
            let actual = type_to_variable(subs, typ);
            let expected = type_to_variable(subs, expected_type.get_type());

            subs.union(actual, expected);
        },
        Lookup(symbol, expected_type, region) => {
            let actual = subs.copy_var(env.get(&symbol).unwrap_or_else(|| {
                panic!("Could not find symbol {:?} in env {:?}", symbol, env)
            }));
            let expected = type_to_variable(subs, expected_type.get_type());

            subs.union(actual, expected);
        },
        And(sub_constraints) => {
            for sub_constraint in sub_constraints {
                solve(env, subs, sub_constraint);
            }
        },
        Let(box_let_constraint) => {
            let let_con = *box_let_constraint;

            match let_con.ret_constraint {
                True => {
                    // If the return expression is guaranteed to solve,
                    // solve the assignments themselves and move on.
                    solve(env, subs, let_con.assignments_constraint)
                },
                ret_con => {
                    // Solve the assignments' constraints first.
                    solve(env, subs, let_con.assignments_constraint);

                    // Add a variable for each assignment to the env.
                    let mut new_env = env.clone();

                    for (symbol, loc_type) in let_con.assignment_types {
                        // We must not overwrite existing symbols! If we do,
                        // we will overwrite procedure entries, which were
                        // inserted earlier in solving. (If we allowed 
                        // shadowing, we'd need to do something fancier here.)
                        if !new_env.contains_key(&symbol) {
                            let var = type_to_variable(subs, loc_type.value);

                            new_env.insert(symbol, var);
                        }
                    }

                    // Now solve the body, using the new env which includes
                    // the assignments' name-to-variable mappings.
                    solve(&new_env, subs, ret_con);

                    // TODO do an occurs check for each of the assignments!
                }
            }
        },
    }
}

fn type_to_variable(subs: &mut Subs, typ: Type) -> Variable {
    match typ {
        Variable(var) => var,
        Apply(module_name, name, arg_types) => {
            let args: Vec<Variable> =
                arg_types.into_iter()
                    .map(|arg| type_to_variable(subs, arg))
                    .collect();

            let flat_type = FlatType::Apply(module_name, name, args);
            let content = Content::Structure(flat_type);

            subs.fresh(Descriptor::from(content))
        },
        EmptyRec => {
            let content = Content::Structure(FlatType::EmptyRecord);

            subs.fresh(Descriptor::from(content))
        },
        Function(arg_types, ret_type) => {
            let arg_vars = arg_types.into_iter().map(|arg_type| {
                type_to_variable(subs, arg_type)
            }).collect();
            let ret_var = type_to_variable(subs, *ret_type);
            let content = Content::Structure(FlatType::Func(arg_vars, ret_var));

            subs.fresh(Descriptor::from(content))
        },
        Operator(box_type) => {
            let op_type = *box_type;
            let l_var = type_to_variable(subs, op_type.left);
            let r_var = type_to_variable(subs, op_type.right);
            let ret_var = type_to_variable(subs, op_type.ret);
            let content = Content::Structure(FlatType::Operator(l_var, r_var, ret_var));

            subs.fresh(Descriptor::from(content))
        },
        Erroneous(problem) => {
            let content = Content::Structure(FlatType::Erroneous(problem));

            subs.fresh(Descriptor::from(content))
        }
    }
}

