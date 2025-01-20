use crate::help_parse::ParseExpr;
use bumpalo::Bump;
use roc_can::{
    env::Env,
    expr::{canonicalize_expr, Expr, Output},
    scope::Scope,
};
use roc_can_solo::env::SoloEnv;
use roc_can_solo::scope::SoloScope;
use roc_module::symbol::{IdentIds, Interns, ModuleId, ModuleIds, PackageModuleIds, Symbol};
use roc_problem::can::Problem;
use roc_region::all::{Loc, Region};
use roc_types::{
    subs::{VarStore, Variable},
    types::{AliasVar, Type},
};
use std::path::Path;

#[derive(Debug)]
pub struct CanExprOut {
    pub expr: Expr,
    pub region: Region,
    pub output: Output,
    pub problems: Vec<Problem>,
    pub home: ModuleId,
    pub interns: Interns,
    pub var_store: VarStore,
    pub var: Variable,
}

pub struct CanExpr {
    pub(crate) home: ModuleId,

    parse_expr: ParseExpr,
}

impl Default for CanExpr {
    fn default() -> Self {
        Self {
            parse_expr: ParseExpr::default(),
            home: test_home(),
        }
    }
}

fn test_home() -> ModuleId {
    ModuleIds::default().get_or_insert(&"Test".into())
}

impl CanExpr {
    pub fn can_expr<'a>(&'a self, input: &'a str) -> CanExprOut {
        match self.parse_expr.parse_loc_expr(input) {
            Ok(loc_expr) => {
                let mut var_store = VarStore::default();
                let var = var_store.fresh();
                let qualified_module_ids = PackageModuleIds::default();
                let home = test_home();

                let mut scope = Scope::new(
                    home,
                    "TestPath".into(),
                    IdentIds::default(),
                    Default::default(),
                );

                let dep_idents = IdentIds::exposed_builtins(0);
                let mut env = Env::new(
                    self.arena(),
                    input,
                    home,
                    Path::new("Test.roc"),
                    &dep_idents,
                    &qualified_module_ids,
                    None,
                );

                // Desugar operators (convert them to Apply calls, taking into account
                // operator precedence and associativity rules), before doing other canonicalization.
                //
                // If we did this *during* canonicalization, then each time we
                // visited a BinOp node we'd recursively try to apply this to each of its nested
                // operators, and then again on *their* nested operators, ultimately applying the
                // rules multiple times unnecessarily.
                let mut solo_env = SoloEnv::new(self.arena(), input, Path::new("Test.roc"));
                let mut solo_scope = SoloScope::new();
                let loc_expr =
                    roc_can_solo::desugar::desugar_expr(&mut solo_env, &mut solo_scope, &loc_expr);

                scope.add_alias(
                    Symbol::NUM_INT,
                    Region::zero(),
                    vec![Loc::at_zero(AliasVar::unbound(
                        "a".into(),
                        Variable::EMPTY_RECORD,
                    ))],
                    vec![],
                    Type::EmptyRec,
                    roc_types::types::AliasKind::Structural,
                );

                let (loc_expr, output) = canonicalize_expr(
                    &mut env,
                    &mut var_store,
                    &mut scope,
                    Region::zero(),
                    &loc_expr.value,
                );

                let mut all_ident_ids = IdentIds::exposed_builtins(1);
                all_ident_ids.insert(home, scope.locals.ident_ids);

                let interns = Interns {
                    module_ids: env.qualified_module_ids.clone().into_module_ids(),
                    all_ident_ids,
                };

                CanExprOut {
                    expr: loc_expr.value,
                    region: loc_expr.region,
                    output,
                    problems: env.problems,
                    home: env.home,
                    var_store,
                    interns,
                    var,
                }
            }
            Err(syntax_error) => {
                panic!("Unexpected syntax error: {:?}", syntax_error);
            }
        }
    }

    pub fn into_arena(self) -> Bump {
        self.parse_expr.into_arena()
    }

    pub fn arena(&self) -> &Bump {
        self.parse_expr.arena()
    }

    pub fn home(&self) -> ModuleId {
        self.home
    }
}
