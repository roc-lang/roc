extern crate bumpalo;

use self::bumpalo::Bump;
use roc_can::desugar;
use roc_can::env::Env;
use roc_can::expr::{canonicalize_expr, Expr};
use roc_can::scope::Scope;
use roc_collections::all::MutMap;
use roc_module::symbol::{IdentIds, Interns, ModuleId, ModuleIds, PackageModuleIds, Symbol};
use roc_problem::can::Problem;
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{AliasVar, Type};
use std::hash::Hash;
use std::path::Path;

pub fn test_home() -> ModuleId {
    ModuleIds::default().get_or_insert(&"Test".into())
}

#[allow(dead_code)]
pub fn can_expr(expr_str: &str) -> CanExprOut {
    can_expr_with(&Bump::new(), test_home(), expr_str)
}

pub struct CanExprOut {
    pub loc_expr: Loc<Expr>,
    pub problems: Vec<Problem>,
    pub interns: Interns,
}

#[allow(dead_code)]
pub fn can_expr_with(arena: &Bump, home: ModuleId, expr_str: &str) -> CanExprOut {
    let loc_expr = roc_parse::test_helpers::parse_loc_with(arena, expr_str).unwrap_or_else(|e| {
        panic!(
            "can_expr_with() got a parse error when attempting to canonicalize:\n\n{expr_str:?} {e:?}"
        )
    });

    let mut var_store = VarStore::default();
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

    let (loc_expr, _) = canonicalize_expr(
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
        problems: env.problems,
        interns,
    }
}

#[allow(dead_code)]
pub fn mut_map_from_pairs<K, V, I>(pairs: I) -> MutMap<K, V>
where
    I: IntoIterator<Item = (K, V)>,
    K: Hash + Eq,
{
    let mut answer = MutMap::default();

    for (key, value) in pairs {
        answer.insert(key, value);
    }

    answer
}
