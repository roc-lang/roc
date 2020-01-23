use crate::can::def::Declaration;
use crate::can::ident::ModuleName;
use crate::module::symbol::Symbol;
use crate::constrain::expr::constrain_decls;
use crate::region::Region;
use crate::subs::Variable;
use crate::types::Constraint;

#[inline(always)]
pub fn constrain_module(
    module_name: ModuleName,
    decls: &[Declaration],
    _lookups: Vec<(Symbol, Variable, Region)>,
) -> Constraint {
    // NOTE lookups are now not included!
    constrain_decls(module_name, &decls)
}
