use canonicalize::{Expr, Procedure, Symbol};
use region::Located;
use subs::{Subs, Content};
use types::Expected::*;
use types::Type::*;
use types::Constraint;
use collections::{ImMap, MutMap};
use solve::solve;
use constrain::{constrain, constrain_procedure};

pub fn infer_expr(subs: &mut Subs, loc_expr: Located<Expr>, procedures: MutMap<Symbol, Procedure>) -> Content {
    let bound_vars = ImMap::default();
    let mut env = ImMap::default();
    let mut constraints = Vec::with_capacity(1 + procedures.len());

    // First add constraints for all the procedures
    for (symbol, proc) in procedures {
        let variable = subs.mk_flex_var();
        let expected = NoExpectation(Variable(variable));

        constraints.push(constrain_procedure(&bound_vars, subs, proc, expected));

        // Record this procedure in the env; variable lookups may reference it!
        env.insert(symbol, variable);
    }

    // Next, constrain the expression.
    let variable = subs.mk_flex_var();
    let expected = NoExpectation(Variable(variable));

    constraints.push(constrain(&bound_vars, subs, loc_expr, expected));

    solve(&env, subs, Constraint::And(constraints));

    subs.get(variable).content
}
