use crate::can::def::Declaration;
use crate::can::ident::Lowercase;
use crate::collections::{ImMap, MutMap, SendMap};
use crate::constrain::expr::constrain_decls;
use crate::module::symbol::{ModuleId, Symbol};
use crate::region::{Located, Region};
use crate::solve::SolvedType;
use crate::subs::{VarId, VarStore, Variable};
use crate::types::{Alias, Constraint, LetConstraint, Type};

#[inline(always)]
pub fn constrain_module(
    home: ModuleId,
    decls: &[Declaration],
    _lookups: Vec<(Symbol, Variable, Region)>,
) -> Constraint {
    // NOTE lookups are now not included!
    constrain_decls(home, &decls)
}

#[derive(Debug, Clone)]
pub struct Import<'a> {
    pub loc_symbol: Located<Symbol>,
    pub solved_type: &'a SolvedType,
}

pub fn constrain_imported_values(
    imports: Vec<Import<'_>>,
    aliases: &MutMap<Symbol, Alias>,
    mut body_con: Constraint,
    var_store: &VarStore,
) -> Constraint {
    // TODO try out combining all the def_types and free_vars, so that we
    // don't need to make this big linked list of nested constraints.
    // Theoretically that should be equivalent to doing it this way!
    for import in imports {
        body_con = constrain_imported_value(
            import.loc_symbol,
            import.solved_type,
            body_con,
            aliases,
            var_store,
        );
    }

    body_con
}

#[derive(Debug, Clone, Default)]
pub struct FreeVars {
    rigid_vars: ImMap<Lowercase, Variable>,
    flex_vars: ImMap<VarId, Variable>,
}

fn to_type(solved_type: &SolvedType, free_vars: &mut FreeVars, var_store: &VarStore) -> Type {
    use crate::solve::SolvedType::*;

    match solved_type {
        Func(args, ret) => {
            let mut new_args = Vec::with_capacity(args.len());

            for arg in args {
                new_args.push(to_type(&arg, free_vars, var_store));
            }

            let new_ret = to_type(&ret, free_vars, var_store);

            Type::Function(new_args, Box::new(new_ret))
        }
        Apply(symbol, args) => {
            let mut new_args = Vec::with_capacity(args.len());

            for arg in args {
                new_args.push(to_type(&arg, free_vars, var_store));
            }

            Type::Apply(*symbol, new_args)
        }
        Rigid(lowercase) => {
            if let Some(var) = free_vars.rigid_vars.get(&lowercase) {
                Type::Variable(*var)
            } else {
                let var = var_store.fresh();
                free_vars.rigid_vars.insert(lowercase.clone(), var);
                Type::Variable(var)
            }
        }
        Flex(var_id) => {
            if let Some(var) = free_vars.flex_vars.get(&var_id) {
                Type::Variable(*var)
            } else {
                let var = var_store.fresh();
                free_vars.flex_vars.insert(*var_id, var);
                Type::Variable(var)
            }
        }
        Wildcard => Type::Variable(var_store.fresh()),
        Record { fields, ext } => {
            let mut new_fields = SendMap::default();

            for (label, typ) in fields {
                new_fields.insert(label.clone(), to_type(&typ, free_vars, var_store));
            }

            Type::Record(new_fields, Box::new(to_type(ext, free_vars, var_store)))
        }
        EmptyRecord => Type::EmptyRec,
        EmptyTagUnion => Type::EmptyTagUnion,
        TagUnion(tags, ext) => {
            let mut new_tags = Vec::with_capacity(tags.len());

            for (tag_name, args) in tags {
                let mut new_args = Vec::with_capacity(args.len());

                for arg in args.iter() {
                    new_args.push(to_type(arg, free_vars, var_store));
                }

                new_tags.push((tag_name.clone(), new_args));
            }

            Type::TagUnion(new_tags, Box::new(to_type(ext, free_vars, var_store)))
        }
        RecursiveTagUnion(rec_var_id, tags, ext) => {
            let mut new_tags = Vec::with_capacity(tags.len());

            for (tag_name, args) in tags {
                let mut new_args = Vec::with_capacity(args.len());

                for arg in args.iter() {
                    new_args.push(to_type(arg, free_vars, var_store));
                }

                new_tags.push((tag_name.clone(), new_args));
            }

            let rec_var = free_vars
                .flex_vars
                .get(rec_var_id)
                .expect("rec var not in flex vars");

            Type::RecursiveTagUnion(
                *rec_var,
                new_tags,
                Box::new(to_type(ext, free_vars, var_store)),
            )
        }
        Boolean(val) => Type::Boolean(val.clone()),
        Alias(symbol, solved_type_variables, solved_actual) => {
            let mut type_variables = Vec::with_capacity(solved_type_variables.len());
            // solved_type_variables consists of Lowercase values,
            // but what we need are Variable values.
            //
            // Generate a fresh Variable for each of them, and
            // incorporate them into free_vars
            //
            // Clone the existing rigid_vars; we don't want to modify them
            // when introducing these (since they only apply within the
            // type being aliased) but we still want any rigids already in scope.
            let mut rigid_vars = free_vars.rigid_vars.clone();

            for lowercase in solved_type_variables {
                let generated_var = match rigid_vars.get(&lowercase) {
                    Some(var) => *var,
                    None => {
                        let var = var_store.fresh();

                        rigid_vars.insert(lowercase.clone(), var);

                        var
                    }
                };

                type_variables.push((lowercase.clone(), Type::Variable(generated_var)));
            }

            let mut free_vars = FreeVars {
                rigid_vars,
                flex_vars: free_vars.flex_vars.clone(),
            };
            let actual = to_type(solved_actual, &mut free_vars, var_store);

            Type::Alias(*symbol, type_variables, Box::new(actual))
        }
        Error => {
            panic!("TODO convert from SolvedType::Error to Type somehow");
        }
        Erroneous(problem) => Type::Erroneous(problem.clone()),
    }
}

fn constrain_imported_value(
    loc_symbol: Located<Symbol>,
    solved_type: &SolvedType,
    body_con: Constraint,
    aliases: &MutMap<Symbol, Alias>,
    var_store: &VarStore,
) -> Constraint {
    use Constraint::*;
    let mut free_vars = FreeVars::default();

    // an imported symbol can be both an alias and a value
    match solved_type {
        SolvedType::Alias(symbol, _, _) if symbol == &loc_symbol.value => {
            if let Some(alias) = aliases.get(symbol) {
                constrain_imported_alias(loc_symbol, alias, body_con, var_store)
            } else {
                panic!("Alias {:?} is not available", symbol)
            }
        }
        _ => {
            let mut def_types = SendMap::default();
            let typ = to_type(solved_type, &mut free_vars, var_store);

            def_types.insert(
                loc_symbol.value,
                Located {
                    region: loc_symbol.region,
                    value: typ,
                },
            );

            let mut rigid_vars = Vec::new();

            for (_, var) in free_vars.rigid_vars {
                rigid_vars.push(var);
            }

            Let(Box::new(LetConstraint {
                // rigids from other modules should not be treated as rigid
                // within this module; rather, they should be treated as flex
                rigid_vars,
                flex_vars: Vec::new(),
                // Importing a value doesn't constrain this module at all.
                // All it does is introduce variables and provide def_types for lookups
                def_types,
                def_aliases: SendMap::default(),
                defs_constraint: True,
                ret_constraint: body_con,
            }))
        }
    }
}

fn constrain_imported_alias(
    loc_symbol: Located<Symbol>,
    imported_alias: &Alias,
    body_con: Constraint,
    var_store: &VarStore,
) -> Constraint {
    use Constraint::*;
    let mut def_aliases = SendMap::default();

    let mut vars = Vec::with_capacity(imported_alias.vars.len());
    let mut substitution = ImMap::default();
    for Located {
        region,
        value: (lowercase, old_var),
    } in &imported_alias.vars
    {
        let new_var = var_store.fresh();
        vars.push(Located::at(*region, (lowercase.clone(), new_var)));
        substitution.insert(*old_var, Type::Variable(new_var));
    }

    let mut actual = imported_alias.typ.clone();
    actual.substitute(&substitution);

    let alias = Alias {
        vars,
        region: loc_symbol.region,
        typ: actual,
    };

    def_aliases.insert(loc_symbol.value, alias);

    Let(Box::new(LetConstraint {
        // rigids from other modules should not be treated as rigid
        // within this module; rather, they should be treated as flex
        rigid_vars: Vec::new(),
        flex_vars: Vec::new(),
        // Importing a value doesn't constrain this module at all.
        // All it does is introduce variables and provide def_types for lookups
        def_types: SendMap::default(),
        def_aliases,
        defs_constraint: True,
        ret_constraint: body_con,
    }))
}
