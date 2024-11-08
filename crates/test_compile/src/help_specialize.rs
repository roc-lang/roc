use crate::SolvedExpr;
use bumpalo::Bump;
use roc_specialize_types::{
    DebugInfo, Env, MonoCache, MonoExprId, MonoExprs, MonoTypes, Problem, RecordFieldIds,
    TupleElemIds,
};

#[derive(Debug)]
pub struct SpecializedExprOut {
    pub mono_expr_id: Option<MonoExprId>,
    pub mono_types: MonoTypes,
    pub mono_exprs: MonoExprs,
    pub problems: Vec<Problem>,
}

#[derive(Default)]
pub struct SpecializedExpr {
    solved_expr: SolvedExpr,
}

impl SpecializedExpr {
    pub fn specialize_expr<'a>(&'a self, input: &'a str) -> SpecializedExprOut {
        let mut solved_out = self.solved_expr.solve_expr(input);
        let mut problems = Vec::new();
        let mut debug_info: Option<DebugInfo> = None;
        let mut types_cache = MonoCache::from_subs(&solved_out.subs);
        let mut mono_types = MonoTypes::new();
        let mut mono_exprs = MonoExprs::new();

        let mut env = Env::new(
            &mut solved_out.subs,
            &mut types_cache,
            &mut mono_types,
            &mut mono_exprs,
            RecordFieldIds::default(),
            TupleElemIds::default(),
            &mut debug_info,
            &mut problems,
        );

        let mono_expr_id = env.to_mono_expr(solved_out.expr);

        SpecializedExprOut {
            mono_expr_id,
            problems,
            mono_types,
            mono_exprs,
        }
    }

    pub fn into_arena(self) -> Bump {
        self.solved_expr.into_arena()
    }

    pub fn arena(&self) -> &Bump {
        &self.solved_expr.arena()
    }
}
