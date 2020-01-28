use crate::can::def::Declaration;
use crate::constrain::expr::constrain_decls;
use crate::module::symbol::{ModuleId, Symbol};
use crate::region::Region;
use crate::subs::Variable;
use crate::types::Constraint;

#[inline(always)]
pub fn constrain_module(
    home: ModuleId,
    decls: &[Declaration],
    _lookups: Vec<(Symbol, Variable, Region)>,
) -> Constraint {
    // NOTE lookups are now not included!
    constrain_decls(home, &decls)
}
