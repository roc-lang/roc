use crate::can::symbol::Symbol;
use crate::collections::ImMap;
use crate::subs::{Content, Descriptor, FlatType, Subs, Variable};
use crate::types::Constraint::{self, *};
use crate::types::Type::{self, *};
use crate::unify::unify;

type Env = ImMap<Symbol, Variable>;

pub fn solve(vars_by_symbol: &Env, subs: &mut Subs, constraint: &Constraint) {
    match constraint {
        True => (),
        Eq(typ, expected_type, _region) => {
            // TODO use region?
            let actual = type_to_var(subs, typ.clone());
            let expected = type_to_var(subs, expected_type.clone().get_type());

            unify(subs, actual, expected);
        }
        Lookup(symbol, expected_type, _region) => {
            // TODO use region?
            let actual = subs.copy_var(*vars_by_symbol.get(&symbol).unwrap_or_else(|| {
                // TODO Instead of panicking, solve this as True and record
                // a Problem ("module Foo does not expose `bar`") for later.
                panic!(
                    "Could not find symbol {:?} in vars_by_symbol {:?}",
                    symbol, vars_by_symbol
                )
            }));
            let expected = type_to_var(subs, expected_type.clone().get_type());

            unify(subs, actual, expected);
        }
        And(sub_constraints) => {
            for sub_constraint in sub_constraints.iter() {
                solve(vars_by_symbol, subs, sub_constraint);
            }
        }
        Pattern(_region, _category, typ, expected) => {
            // TODO use region?
            let actual = type_to_var(subs, typ.clone());
            let expected = type_to_var(subs, expected.clone().get_type());

            unify(subs, actual, expected);
        }
        Let(let_con) => {
            match &let_con.ret_constraint {
                True => {
                    // If the return expression is guaranteed to solve,
                    // solve the assignments themselves and move on.
                    solve(vars_by_symbol, subs, &let_con.defs_constraint)
                }
                ret_con => {
                    // Solve the assignments' constraints first.
                    solve(vars_by_symbol, subs, &let_con.defs_constraint);

                    // Add a variable for each assignment to the vars_by_symbol.
                    let mut new_vars_by_symbol = vars_by_symbol.clone();

                    for (symbol, loc_type) in let_con.def_types.iter() {
                        // No need to overwrite existing symbols; it would have no effect.
                        // (If we allowed shadowing, we'd need to do something fancier here.)
                        if !new_vars_by_symbol.contains_key(&symbol) {
                            let var = type_to_var(subs, loc_type.value.clone());

                            new_vars_by_symbol.insert(symbol.clone(), var);
                        }
                    }

                    // Now solve the body, using the new vars_by_symbol which includes
                    // the assignments' name-to-variable mappings.
                    solve(&new_vars_by_symbol, subs, &ret_con);

                    // TODO do an occurs check for each of the assignments!
                }
            }
        }
    }
}

fn type_to_var(subs: &mut Subs, typ: Type) -> Variable {
    type_to_variable(subs, &ImMap::default(), typ)
}

fn type_to_variable(subs: &mut Subs, aliases: &ImMap<Box<str>, Variable>, typ: Type) -> Variable {
    match typ {
        Variable(var) => var,
        Apply {
            module_name,
            name,
            args,
        } => {
            let mut arg_vars = Vec::with_capacity(args.len());

            for arg in args {
                arg_vars.push(type_to_variable(subs, aliases, arg.clone()))
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
                arg_vars.push(type_to_variable(subs, aliases, arg.clone()))
            }

            let ret_var = type_to_variable(subs, aliases, *ret_type);
            let content = Content::Structure(FlatType::Func(arg_vars, ret_var));

            subs.fresh(Descriptor::from(content))
        }
        Erroneous(problem) => {
            let content = Content::Structure(FlatType::Erroneous(problem));

            subs.fresh(Descriptor::from(content))
        }
    }
}
