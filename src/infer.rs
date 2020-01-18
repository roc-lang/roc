use crate::collections::{MutMap, SendMap};
use crate::solve::{self, Solved};
use crate::subs::{Content, Subs, Variable};
use crate::types::{Constraint, Problem};

pub fn infer_expr(
    subs: Subs,
    problems: &mut Vec<Problem>,
    constraint: &Constraint,
    expr_var: Variable,
) -> (Content, Solved<Subs>) {
    let (solved, _) = solve::run(
        &SendMap::default(),
        MutMap::default(),
        problems,
        subs,
        constraint,
    );

    let content = solved.inner().get_without_compacting(expr_var).content;

    (content, solved)
}
