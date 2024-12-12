use crate::CanExpr;
use bumpalo::Bump;
use roc_can::{
    constraint::{Constraint, Constraints},
    expected::Expected,
    expr::Expr,
};
use roc_constrain::expr::{constrain_expr, Env};
use roc_module::symbol::ModuleId;
use roc_region::all::Region;
use roc_types::{subs::Variable, types::Types};

#[derive(Debug)]
pub struct ConstrainedExprOut {
    pub expr: Expr,
    pub var: Variable,
    pub constraint: Constraint,
    pub constraints: Constraints,
    pub types: Types,
    pub region: Region,
}

#[derive(Default)]
pub struct ConstrainedExpr {
    can_expr: CanExpr,
}

impl ConstrainedExpr {
    pub fn constrain_expr<'a>(&'a self, input: &'a str) -> ConstrainedExprOut {
        let mut can_expr_out = self.can_expr.can_expr(input);

        assert_eq!(
            can_expr_out.problems,
            Vec::new(),
            "Encountered unexpected canonicalization problem when trying to constrain expr"
        );

        let mut types = Types::default();
        let mut constraints = Constraints::default();
        let var = can_expr_out.var_store.fresh();
        let var_index = constraints.push_variable(var);
        let expected = constraints.push_expected_type(Expected::NoExpectation(var_index));
        let mut env = Env::new(self.can_expr.home);

        let constraint = constrain_expr(
            &mut types,
            &mut constraints,
            &mut env,
            can_expr_out.region,
            &can_expr_out.expr,
            expected,
        );

        ConstrainedExprOut {
            expr: can_expr_out.expr,
            var: can_expr_out.var,
            constraint,
            constraints,
            types,
            region: can_expr_out.region,
        }
    }

    pub fn into_arena(self) -> Bump {
        self.can_expr.into_arena()
    }

    pub fn arena(&self) -> &Bump {
        self.can_expr.arena()
    }

    pub fn home(&self) -> ModuleId {
        self.can_expr.home()
    }
}
