use crate::can::ident::Lowercase;
use crate::can::symbol::Symbol;
use crate::collections::ImMap;
use crate::region::Located;
use crate::subs::{Content, Descriptor, FlatType, Subs, Variable};
use crate::types::Constraint::{self, *};
use crate::types::Problem;
use crate::types::Type::{self, *};
use crate::unify::{unify, Problems};

type Env = ImMap<Symbol, Variable>;

pub fn solve(
    vars_by_symbol: &Env,
    problems: &mut Problems,
    subs: &mut Subs,
    constraint: &Constraint,
) {
    match constraint {
        True => (),
        Eq(typ, expected_type, _region) => {
            // TODO use region?
            let actual = type_to_var(subs, typ.clone());
            let expected = type_to_var(subs, expected_type.clone().get_type());

            unify(subs, problems, actual, expected);
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

            unify(subs, problems, actual, expected);
        }
        And(sub_constraints) => {
            for sub_constraint in sub_constraints.iter() {
                solve(vars_by_symbol, problems, subs, sub_constraint);
            }
        }
        Pattern(_region, _category, typ, expected) => {
            // TODO use region?
            let actual = type_to_var(subs, typ.clone());
            let expected = type_to_var(subs, expected.clone().get_type());

            unify(subs, problems, actual, expected);
        }
        Let(let_con) => {
            match &let_con.ret_constraint {
                True => {
                    // If the return expression is guaranteed to solve,
                    // solve the assignments themselves and move on.
                    solve(vars_by_symbol, problems, subs, &let_con.defs_constraint)
                }
                ret_con => {
                    // Add a variable for each assignment to the vars_by_symbol.
                    let mut locals = ImMap::default();

                    for (symbol, loc_type) in let_con.def_types.iter() {
                        let var = type_to_var(subs, loc_type.value.clone());

                        locals.insert(
                            symbol.clone(),
                            Located {
                                value: var,
                                region: loc_type.region,
                            },
                        );
                    }

                    // Solve the assignments' constraints first.
                    solve(vars_by_symbol, problems, subs, &let_con.defs_constraint);

                    let mut new_vars_by_symbol = vars_by_symbol.clone();

                    for (symbol, loc_var) in locals.iter() {
                        new_vars_by_symbol.insert(symbol.clone(), loc_var.value);
                    }

                    // Now solve the body, using the new vars_by_symbol which includes
                    // the assignments' name-to-variable mappings.
                    solve(&new_vars_by_symbol, problems, subs, &ret_con);

                    for (symbol, loc_var) in locals {
                        check_for_infinite_type(subs, problems, symbol, loc_var);
                    }
                }
            }
        }
    }
}

fn type_to_var(subs: &mut Subs, typ: Type) -> Variable {
    type_to_variable(subs, &ImMap::default(), typ)
}

fn type_to_variable(subs: &mut Subs, aliases: &ImMap<Lowercase, Variable>, typ: Type) -> Variable {
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
        Record(fields, ext) => {
            let mut field_vars = ImMap::default();

            for (field, field_type) in fields {
                field_vars.insert(field, type_to_variable(subs, aliases, field_type));
            }

            let ext_var = type_to_variable(subs, aliases, *ext);
            let content = Content::Structure(FlatType::Record(field_vars, ext_var));

            subs.fresh(Descriptor::from(content))
        }
        Alias(home, name, args, alias_type) => {
            let mut arg_vars = Vec::with_capacity(args.len());
            let mut new_aliases = ImMap::default();

            for (arg, arg_type) in args {
                let arg_var = type_to_variable(subs, aliases, arg_type.clone());

                arg_vars.push((arg.clone(), arg_var));
                new_aliases.insert(arg.into(), arg_var);
            }

            let alias_var = type_to_variable(subs, &new_aliases, *alias_type);
            let content = Content::Alias(home, name, arg_vars, alias_var);

            subs.fresh(Descriptor::from(content))
        }
        Erroneous(problem) => {
            let content = Content::Structure(FlatType::Erroneous(problem));

            subs.fresh(Descriptor::from(content))
        }
    }
}

fn check_for_infinite_type(
    subs: &mut Subs,
    problems: &mut Problems,
    symbol: Symbol,
    loc_var: Located<Variable>,
) {
    let var = loc_var.value;

    if subs.occurs(var) {
        let error_type = subs.to_error_type(var);
        let problem = Problem::CircularType(symbol, error_type, loc_var.region);

        subs.set_content(var, Content::Error(problem.clone()));

        problems.push(problem);
    }
}
