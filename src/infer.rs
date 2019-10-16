use can::expr::Expr;
use can::procedure::Procedure;
use can::symbol::Symbol;
use collections::{ImMap, MutMap};
use constrain::{constrain, constrain_procedure};
use region::Located;
use solve::solve;
use subs::{Content, Subs};
use types::Constraint;
use types::Expected::*;
use types::Type::*;

pub fn infer_expr<'a>(
    subs: &'a mut Subs<'a>,
    loc_expr: Located<Expr<'a>>,
    procedures: MutMap<Symbol<'a>, Procedure<'a>>,
) -> Content<'a> {
    panic!("TODO re-constrain procedures.");
    // let bound_vars = ImMap::default();
    // let mut env = ImMap::default();
    // let mut constraints = Vec::with_capacity(1 + procedures.len());

    // First add constraints for all the procedures
    // for (symbol, proc) in procedures {
    //     let variable = subs.mk_flex_var();
    //     let expected = NoExpectation(Variable(variable));

    //     constraints.push(constrain_procedure(&bound_vars, subs, proc, expected));

    //     // Record this procedure in the env; variable lookups may reference it!
    //     env.insert(symbol, variable);
    // }

    // Next, constrain the expression.
    // let variable = subs.mk_flex_var();
    // let expected = NoExpectation(Variable(variable));
    // let constraint = constrain(&bound_vars, subs, loc_expr, expected);

    // constraints.push(constraint);
    //
    // solve(&env, subs, &Constraint::And(constraints.into_bump_slice()));

    // subs.get(variable).content
}
