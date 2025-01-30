use crate::help_constrain::ConstrainedExpr;
use bumpalo::Bump;
use roc_can::{
    abilities::AbilitiesStore,
    expr::{Expr, PendingDerives},
    module::ExposedByModule,
};
use roc_collections::VecMap;
use roc_derive::SharedDerivedModule;
use roc_load::FunctionKind;
use roc_module::symbol::ModuleId;
use roc_region::all::Region;
use roc_solve::{
    module::{SolveConfig, Solved},
    solve, Aliases,
};
use roc_solve_problem::TypeError;
use roc_types::subs::{Subs, Variable};

#[derive(Debug)]
pub struct SolvedExprOut {
    pub expr: Expr,
    pub region: Region,
    pub problems: Vec<TypeError>,
    pub var: Variable,
    pub subs: Solved<Subs>,
}

#[derive(Default)]
pub struct SolvedExpr {
    constrained_expr: ConstrainedExpr,
}

impl SolvedExpr {
    pub fn solve_expr<'a>(&'a self, input: &'a str) -> SolvedExprOut {
        let constrained_expr_out = self.constrained_expr.constrain_expr(input);
        let mut problems = Vec::new();
        let mut aliases = Aliases::default();
        let subs = Subs::new();
        let mut abilities_store = AbilitiesStore::default();
        let solve_config = SolveConfig {
            home: self.constrained_expr.home(),
            constraints: &constrained_expr_out.constraints,
            root_constraint: constrained_expr_out.constraint,
            types: constrained_expr_out.types,
            function_kind: FunctionKind::LambdaSet,
            pending_derives: PendingDerives::default(),
            exposed_by_module: &ExposedByModule::default(),
            derived_module: SharedDerivedModule::default(),
            module_params: None,
            module_params_vars: VecMap::default(),
            host_exposed_symbols: None,
            #[cfg(debug_assertions)]
            checkmate: None,
        };

        let solve_output = solve::run(
            solve_config,
            &mut problems,
            subs,
            &mut aliases,
            &mut abilities_store,
        );

        SolvedExprOut {
            expr: constrained_expr_out.expr,
            region: constrained_expr_out.region,
            problems,
            var: constrained_expr_out.var,
            subs: solve_output.solved,
        }
    }

    pub fn into_arena(self) -> Bump {
        self.constrained_expr.into_arena()
    }

    pub fn arena(&self) -> &Bump {
        self.constrained_expr.arena()
    }

    pub fn home(&self) -> ModuleId {
        self.constrained_expr.home()
    }
}
