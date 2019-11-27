use can::symbol::Symbol;
use collections::ImMap;
use solve::solve;
use subs::{Content, Subs, Variable};
use types::Constraint;

pub fn infer_expr(subs: &mut Subs, constraint: &Constraint, expr_var: Variable) -> Content {
    let env: ImMap<Symbol, Variable> = ImMap::default();

    solve(&env, subs, constraint);

    subs.get(expr_var).content
}
