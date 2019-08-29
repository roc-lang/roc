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
use types::Constraint::{self, *};
use types::Type::{self, *};

type Env = ImMap<String, Variable>;

pub fn solve(env: &Env, subs: &mut Subs, constraint: Constraint) {
    println!("\nSolving:\n\n\t{:?}\n\n", constraint);
    match constraint {
        True => (),
        Eq(typ, expected_type, region) => {
            let actual = type_to_variable(subs, typ);
            let expected = type_to_variable(subs, expected_type.unwrap());

            subs.union(actual, expected);
        },
        And(sub_constraints) => {
            for sub_constraint in sub_constraints {
                solve(env, subs, sub_constraint);
            }
        },
        Let(box_let_constraint) => {
            let let_con = *box_let_constraint;
            let no_rigid_vars = let_con.rigid_vars.is_empty();

            match let_con.ret_constraint {
                True if no_rigid_vars => {
                    // If the return expression is guaranteed to solve,
                    // and there are no rigid vars to worry about, 
                    // solve the assignments themselves and move on.
                    solve(env, subs, let_con.assignments_constraint)
                },
                body_con => {
                    if no_rigid_vars && let_con.flex_vars.is_empty() {
                        // Solve the assignments' constraints first.
                        solve(env, subs, let_con.assignments_constraint);

                        // Add a variable for each assignment to the env.
                        let new_env = env.clone();

                        for (name, loc_type) in let_con.assignment_types {
                            let var = type_to_variable(subs, loc_type.value);

                            new_env.insert(name, var);
                        }

                        // Now solve the body, using the new env which includes
                        // the assignments' name-to-variable mappings.
                        solve(&new_env, subs, let_con.ret_constraint);

                        // TODO do an occurs check for each of the assignments!
                    } else {
                        let vars = let_con.rigid_vars;

                        vars.extend(let_con.flex_vars);

                        // Add a variable for each assignment to the env.
                        let new_env = env.clone();

                        for (name, loc_type) in let_con.assignment_types {
                            let var = type_to_variable(subs, loc_type.value);

                            new_env.insert(name, var);
                        }
                    }
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
        _ => panic!("TODO type_to_var")

        // AppN home name args ->
        // do  argVars <- traverse go args
        //     register rank pools (Structure (App1 home name argVars))

        // FunN a b ->
        // do  aVar <- go a
        //     bVar <- go b
        //     register rank pools (Structure (Fun1 aVar bVar))
    }
}

