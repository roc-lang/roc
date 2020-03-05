use crate::solve::{self, Solved};
use crate::subs::{Content, Subs, Variable};
use crate::types::{Constraint, Problem};
use roc_collections::all::{MutMap, SendMap};

pub fn infer_expr(
    subs: Subs,
    problems: &mut Vec<Problem>,
    constraint: &Constraint,
    expr_var: Variable,
) -> (Content, Solved<Subs>) {
    let env = solve::Env {
        aliases: MutMap::default(),
        vars_by_symbol: SendMap::default(),
    };
    let (solved, _) = solve::run(&env, problems, subs, constraint);

    let content = solved.inner().get_without_compacting(expr_var).content;

    (content, solved)
}
