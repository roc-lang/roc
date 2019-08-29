use canonicalize::{Expr, Procedure, Symbol};
use region::Located;
use subs::{Subs, Content};
use types::Expected::*;
use types::Type::*;
use collections::{ImMap, MutMap};
use solve::solve;
use constrain::constrain;

pub fn infer_expr(subs: &mut Subs, loc_expr: Located<Expr>, procedures: MutMap<Symbol, Procedure>) -> Content {
    let bound_vars = ImMap::default();
    let env = ImMap::default();
    let variable = subs.mk_flex_var();
    let expected = NoExpectation(Variable(variable));
    let constraint = constrain(bound_vars, subs, loc_expr, expected);

    solve(&env, subs, constraint);

    subs.get(variable).content
}
