use can::procedure::Procedure;
use can::symbol::Symbol;
use collections::{ImMap, MutMap};
use solve::solve;
use subs::{Content, Subs, Variable};
use types::Constraint;

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

    subs.get(dbg!(expr_var)).content
}
