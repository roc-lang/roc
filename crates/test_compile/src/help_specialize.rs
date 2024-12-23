use crate::SolvedExpr;
use bumpalo::Bump;
use roc_region::all::Region;
use roc_specialize_types::{
    DebugInfo, Env, Interns, MonoExprId, MonoExprs, MonoPatterns, MonoTypeCache, MonoTypes,
    Problem, RecordFieldIds, TupleElemIds, WhenBranches,
};

#[derive(Debug)]
pub struct SpecializedExprOut {
    pub mono_expr_id: MonoExprId,
    pub region: Region,
    pub mono_types: MonoTypes,
    pub mono_exprs: MonoExprs,
    pub mono_patterns: MonoPatterns,
    pub when_branches: WhenBranches,
    pub problems: Vec<Problem>,
}

#[derive(Default)]
pub struct SpecializedExpr {
    solved_expr: SolvedExpr,
}

impl SpecializedExpr {
    pub fn specialize_expr<'a>(
        &'a self,
        input: &'a str,
        string_interns: &'a mut Interns<'a>,
    ) -> SpecializedExprOut {
        let mut solved_out = self.solved_expr.solve_expr(input);
        let mut problems = Vec::new();
        let mut debug_info: Option<DebugInfo> = None;
        let mut types_cache = MonoTypeCache::from_solved_subs(&solved_out.subs);
        let mut mono_types = MonoTypes::new();
        let mut mono_exprs = MonoExprs::new();
        let mut mono_patterns = MonoPatterns::new();
        let mut when_branches = WhenBranches::new();

        let mono_expr_id = {
            let mut env = Env::new(
                self.solved_expr.arena(),
                &mut solved_out.subs,
                &mut types_cache,
                &mut mono_types,
                &mut mono_exprs,
                &mut mono_patterns,
                &mut when_branches,
                RecordFieldIds::default(),
                TupleElemIds::default(),
                string_interns,
                &mut debug_info,
                &mut problems,
            );

            let mono_expr = env.to_mono_expr(&solved_out.expr);
            mono_exprs.add(mono_expr, Region::zero())
        };

        SpecializedExprOut {
            mono_expr_id,
            region: solved_out.region,
            problems,
            mono_types,
            mono_exprs,
            mono_patterns,
            when_branches,
        }
    }

    pub fn into_arena(self) -> Bump {
        self.solved_expr.into_arena()
    }

    pub fn arena(&self) -> &Bump {
        self.solved_expr.arena()
    }
}
