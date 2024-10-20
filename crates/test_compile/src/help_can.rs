use crate::help_parse::ParseExpr;
use bumpalo::Bump;
use roc_can::expr::Expr;

pub struct CanExpr {
    parse_expr: ParseExpr,
}

impl Default for CanExpr {
    fn default() -> Self {
        Self {
            parse_expr: ParseExpr::default(),
        }
    }
}

impl CanExpr {
    pub fn can_expr<'a>(&'a self, input: &'a str) -> Result<Expr, CanExprProblem> {
        match self.parse_expr.parse_expr(input) {
            Ok(ast) => {
                // todo canonicalize AST and return that result.
                let loc_expr = roc_parse::test_helpers::parse_loc_with(arena, expr_str).unwrap_or_else(|e| {
                    panic!(
                        "can_expr_with() got a parse error when attempting to canonicalize:\n\n{expr_str:?} {e:?}"
                    )
                });

                let mut var_store = VarStore::default();
                let var = var_store.fresh();
                let qualified_module_ids = PackageModuleIds::default();

                let mut scope = Scope::new(
                    home,
                    "TestPath".into(),
                    IdentIds::default(),
                    Default::default(),
                );

                let dep_idents = IdentIds::exposed_builtins(0);
                let mut env = Env::new(
                    arena,
                    expr_str,
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
                let loc_expr = desugar::desugar_expr(&mut env, &mut scope, &loc_expr);

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
                    loc_expr,
                    output,
                    problems: env.problems,
                    home: env.home,
                    var_store,
                    interns,
                    var,
                }
            }
            Err(syntax_error) => {
                // todo panic due to unexpected syntax error
            }
        }
    }

    pub fn into_arena(self) -> Bump {
        self.parse_expr.into_arena()
    }
}
