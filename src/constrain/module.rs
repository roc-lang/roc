use crate::can::def::Declaration;
use crate::collections::{ImMap, SendMap};
use crate::constrain::expr::constrain_decls;
use crate::module::symbol::{ModuleId, Symbol};
use crate::region::{Located, Region};
use crate::solve::SolvedType;
use crate::subs::{VarStore, Variable};
use crate::types::Expected::*;
use crate::types::{Constraint, LetConstraint, Type};

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
    mut body_con: Constraint,
    var_store: &VarStore,
) -> Constraint {
    // TODO try out combining all the def_types and free_vars, so that we
    // don't need to make this big linked list of nested constraints.
    // Theoretically that should be equivalent to doing it this way!
    for import in dbg!(imports) {
        body_con =
            constrain_imported_value(import.loc_symbol, import.solved_type, body_con, var_store);
    }

    body_con
}

fn to_type(solved_type: &SolvedType, free_vars: &mut Vec<Variable>, var_store: &VarStore) -> Type {
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
        Rigid(symbol) => {
            // TODO: I guess the design here is that we need to record
            // rigids after all, and incorporate them into
            // the final constraint? Just to make sure the types
            // actually line up? Should take a look at how
            // this is done in the Defs annotations.
            panic!("TODO to_type for a rigid: {:?}", symbol);
        }
        Wildcard => {
            panic!("TODO flex variable for wildcard");
        }
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
        Boolean(val) => Type::Boolean(val.clone()),
        Alias(_, _, _) => {
            panic!("TODO convert from SolvedType::Alias to Type::Alias");
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
    var_store: &VarStore,
) -> Constraint {
    use Constraint::*;

    let mut def_types = SendMap::default();
    let mut free_vars = Vec::new();
    let typ = to_type(solved_type, &mut free_vars, var_store);

    def_types.insert(
        loc_symbol.value,
        Located {
            region: loc_symbol.region,
            value: typ,
        },
    );

    Let(Box::new(LetConstraint {
        // rigids from other modules should not be treated as rigid
        // within this module; rather, they should be treated as flex
        rigid_vars: Vec::new(),
        flex_vars: free_vars,
        // Importing a value doesn't constrain this module at all.
        // All it does is introduce variables and provide def_types for lookups
        def_types,
        defs_constraint: True,
        ret_constraint: body_con,
    }))
}
