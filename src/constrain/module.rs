use crate::can::def::Declaration;
use crate::collections::SendMap;
use crate::constrain::expr::constrain_decls;
use crate::module::symbol::{ModuleId, Symbol};
use crate::region::{Located, Region};
use crate::solve::SolvedType;
use crate::subs::{VarStore, Variable};
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

pub struct Import {
    pub loc_symbol: Located<Symbol>,
    pub solved_type: SolvedType,
}

pub fn constrain_imported_values(
    imports: Vec<Import>,
    mut body_con: Constraint,
    var_store: &VarStore,
) -> Constraint {
    // TODO try out combining all the def_types and free_vars, so that we
    // don't need to make this big linked list of nested constraints.
    // Theoretically that should be equivalent to doing it this way!
    for import in imports {
        body_con =
            constrain_imported_value(import.loc_symbol, import.solved_type, body_con, var_store);
    }

    body_con
}

fn to_type(solved_type: SolvedType, free_vars: &mut Vec<Variable>, var_store: &VarStore) -> Type {
    panic!("TODO implement to_type");
}

fn constrain_imported_value(
    loc_symbol: Located<Symbol>,
    solved_type: SolvedType,
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
