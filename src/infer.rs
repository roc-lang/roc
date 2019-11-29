use crate::can::symbol::Symbol;
use crate::collections::ImMap;
use crate::solve::solve;
use crate::subs::{Content, Subs, Variable};
use crate::types::Constraint;

pub fn infer_expr(subs: &mut Subs, constraint: &Constraint, expr_var: Variable) -> Content {
    let env: ImMap<Symbol, Variable> = ImMap::default();

    solve(&env, subs, constraint);

    subs.get(expr_var).content
}
