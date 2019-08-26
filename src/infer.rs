use canonicalize::Expr;
use region::Located;
use subs::{Subs, Content};
use types::Expected::*;
use types::Type::*;
use collections::ImMap;
use solve::solve;
use constrain::constrain;

pub fn infer(loc_expr: Located<Expr>) -> Content {
    let mut subs = Subs::new();
    let bound_vars = ImMap::default();
    let variable = subs.mk_flex_var();
    let expected = NoExpectation(Variable(variable));
    let constraint = constrain(bound_vars, &mut subs, loc_expr, expected);

    solve(&mut subs, constraint);

    subs.get(variable).content
}

