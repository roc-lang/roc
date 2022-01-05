extern crate bumpalo;

use self::bumpalo::Bump;
use roc_can::env::Env;
use roc_can::expr::Output;
use roc_can::expr::{canonicalize_expr, Expr};
use roc_can::operator;
use roc_can::scope::Scope;
use roc_collections::all::MutMap;
use roc_module::symbol::{IdentIds, Interns, ModuleId, ModuleIds};
use roc_problem::can::Problem;
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};
use std::hash::Hash;

pub fn test_home() -> ModuleId {
    ModuleIds::default().get_or_insert(&"Test".into())
}

#[allow(dead_code)]
pub fn can_expr(expr_str: &str) -> CanExprOut {
    can_expr_with(&Bump::new(), test_home(), expr_str)
}

pub struct CanExprOut {
    pub loc_expr: Loc<Expr>,
    pub output: Output,
    pub problems: Vec<Problem>,
    pub home: ModuleId,
    pub interns: Interns,
    pub var_store: VarStore,
    pub var: Variable,
}

#[allow(dead_code)]
pub fn can_expr_with(arena: &Bump, home: ModuleId, expr_str: &str) -> CanExprOut {
    let loc_expr = roc_parse::test_helpers::parse_loc_with(arena, expr_str).unwrap_or_else(|e| {
        panic!(
            "can_expr_with() got a parse error when attempting to canonicalize:\n\n{:?} {:?}",
            expr_str, e
        )
    });

    let mut var_store = VarStore::default();
    let var = var_store.fresh();
    let module_ids = ModuleIds::default();

    // Desugar operators (convert them to Apply calls, taking into account
    // operator precedence and associativity rules), before doing other canonicalization.
    //
    // If we did this *during* canonicalization, then each time we
    // visited a BinOp node we'd recursively try to apply this to each of its nested
    // operators, and then again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.
    let loc_expr = operator::desugar_expr(arena, &loc_expr);

    let mut scope = Scope::new(home, &mut var_store);
    let dep_idents = IdentIds::exposed_builtins(0);
    let mut env = Env::new(home, &dep_idents, &module_ids, IdentIds::default());
    let (loc_expr, output) = canonicalize_expr(
        &mut env,
        &mut var_store,
        &mut scope,
        Region::zero(),
        &loc_expr.value,
    );

    let mut all_ident_ids = MutMap::default();

    // When pretty printing types, we may need the exposed builtins,
    // so include them in the Interns we'll ultimately return.
    for (module_id, ident_ids) in IdentIds::exposed_builtins(0) {
        all_ident_ids.insert(module_id, ident_ids);
    }

    all_ident_ids.insert(home, env.ident_ids);

    let interns = Interns {
        module_ids: env.module_ids.clone(),
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
