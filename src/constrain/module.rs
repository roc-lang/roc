use crate::can::def::Declaration;
use crate::can::symbol::Symbol;
use crate::collections::{ImMap, SendMap};
use crate::constrain::expr::{constrain_decls, Info};
use crate::region::Region;
use crate::subs::Variable;
use crate::types::Constraint::{self, *};
use crate::types::Expected::*;
use crate::types::LetConstraint;
use crate::types::Type;

#[inline(always)]
pub fn constrain_module(
    decls: &[Declaration],
    lookups: Vec<(Symbol, Variable, Region)>,
) -> Constraint {
    let mut flex_info = Info::default();

    for (symbol, expr_var, region) in lookups {
        // Add the usual Lookup constraint as if this were a normal def.
        let expr_type = Type::Variable(expr_var);
        let expected = NoExpectation(expr_type.clone());

        flex_info.constraints.push(Lookup(symbol, expected, region));
    }

    constrain_decls(
        &ImMap::default(),
        &mut SendMap::default(),
        &decls,
        &mut flex_info,
    );

    Let(Box::new(LetConstraint {
        rigid_vars: Vec::new(),
        flex_vars: flex_info.vars,
        def_types: flex_info.def_types,
        defs_constraint: And(flex_info.constraints),
        ret_constraint: True,
    }))
}
