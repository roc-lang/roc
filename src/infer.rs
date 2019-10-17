use can::procedure::Procedure;
use can::symbol::Symbol;
use collections::{ImMap, MutMap};
use constrain::Constraints;
use solve::solve;
use subs::{Content, Subs, Variable};

pub fn infer_expr(
    subs: &mut Subs,
    procedures: MutMap<Symbol, Procedure>,
    constraints: &Constraints,
    expr_var: Variable,
) -> Content {
    let mut env: ImMap<Symbol, Variable> = ImMap::default();

    for (symbol, proc) in procedures {
        env.insert(symbol, proc.var);
    }

    for constraint in constraints.iter() {
        solve(&env, subs, constraint);
    }

    subs.get(expr_var).content
}
