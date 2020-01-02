use crate::can::symbol::Symbol;
use crate::collections::ImMap;
use crate::solve;
use crate::subs::{Content, Subs, Variable};
use crate::types::{Constraint, Problem};

pub fn infer_expr(
    subs: &mut Subs,
    problems: &mut Vec<Problem>,
    constraint: &Constraint,
    expr_var: Variable,
) -> (Content, ImMap<Symbol, Variable>) {
    let resolved_vars = solve::run(
        &ImMap::default(),
        problems,
        subs,
        constraint,
        ImMap::default(),
    );

    (subs.get(expr_var).content, resolved_vars)
}
