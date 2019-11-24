use crate::can::symbol::Symbol;
use crate::collections::ImMap;
use crate::solve::solve;
use crate::subs::{Content, Subs, Variable};
use crate::types::Constraint;
use crate::uniqueness::Env;

pub fn infer_expr(subs: &mut Subs, constraint: &Constraint, expr_var: Variable) -> Content {
    let env: ImMap<Symbol, Variable> = ImMap::default();

    solve(&env, subs, constraint);

    subs.get(expr_var).content
}

pub fn infer_uniq(
    subs: &mut Subs,
    environment: &Env,
    constraint: &Constraint,
    expr_var: Variable,
) -> Content {
    let mut env: ImMap<Symbol, Variable> = environment.bound_names.clone();

    solve(&env, subs, constraint);

    subs.get(expr_var).content
}
