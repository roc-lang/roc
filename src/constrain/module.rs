use crate::can::def::Def;
use crate::can::symbol::Symbol;
use crate::collections::{ImMap, SendMap};
use crate::constrain::expr::{constrain_defs, Info};
use crate::region::Region;
use crate::subs::Variable;
use crate::types::Constraint::{self, *};
use crate::types::Expected::*;
use crate::types::Type;

#[inline(always)]
pub fn constrain_module(defs: &[Def], lookups: Vec<(Symbol, Variable, Region)>) -> Constraint {
    let mut flex_info = Info::default();

    for (symbol, expr_var, region) in lookups {
        // Add the usual Lookup constraint as if this were a normal def.
        let expr_type = Type::Variable(expr_var);
        let expected = NoExpectation(expr_type.clone());

        flex_info.constraints.push(Lookup(symbol, expected, region));
    }

    constrain_defs(
        &ImMap::default(),
        &mut SendMap::default(),
        &defs,
        &mut flex_info,
    );

    Constraint::And(flex_info.constraints)
}
