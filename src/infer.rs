use crate::can::procedure::Procedure;
use crate::can::symbol::Symbol;
use crate::collections::{ImMap, MutMap};
use crate::solve::solve;
use crate::subs::{Content, Subs, Variable};
use crate::types::Constraint;

pub fn infer_expr(
    subs: &mut Subs,
    procedures: MutMap<Symbol, Procedure>,
    constraint: &Constraint,
    expr_var: Variable,
) -> Content {
    let mut env: ImMap<Symbol, Variable> = ImMap::default();

    for (symbol, proc) in procedures {
        env.insert(symbol, proc.var);
    }

    solve(&env, subs, constraint);

    subs.get(expr_var).content
}
