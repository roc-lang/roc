use crate::can::def::Declaration;
use crate::can::symbol::Symbol;
use crate::collections::SendMap;
use crate::constrain::expr::constrain_decls;
use crate::region::Region;
use crate::subs::Variable;
use crate::types::Constraint;

#[inline(always)]
pub fn constrain_module(
    decls: &[Declaration],
    _lookups: Vec<(Symbol, Variable, Region)>,
) -> Constraint {
    // NOTE lookups are now not included!
    // TODO use rigids in solving
    let mut found_rigids = SendMap::default();
    constrain_decls(&mut found_rigids, &decls)
}
