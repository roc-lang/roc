extern crate bumpalo;

use self::bumpalo::Bump;
use roc_can::constraint::Constraint;
use roc_can::env::Env;
use roc_can::expected::Expected;
use roc_can::expr::{canonicalize_expr, Expr, Output};
use roc_can::operator;
use roc_can::scope::Scope;
use roc_collections::all::{ImMap, MutMap, SendSet};
use roc_constrain::expr::constrain_expr;
use roc_constrain::module::{constrain_imported_values, Import};
use roc_module::symbol::{IdentIds, Interns, ModuleId, ModuleIds};
use roc_parse::parser::{SourceError, SyntaxError};
use roc_problem::can::Problem;
use roc_region::all::Loc;
use roc_solve::solve;
use roc_types::subs::{Content, Subs, VarStore, Variable};
use roc_types::types::Type;
use std::hash::Hash;
use std::path::{Path, PathBuf};

pub fn test_home() -> ModuleId {
    ModuleIds::default().get_or_insert(&"Test".into())
}

#[allow(dead_code)]
pub fn infer_expr(
    subs: Subs,
    problems: &mut Vec<solve::TypeError>,
    constraint: &Constraint,
    expr_var: Variable,
) -> (Content, Subs) {
    let env = solve::Env {
        aliases: MutMap::default(),
        vars_by_symbol: MutMap::default(),
    };
    let (solved, _) = solve::run(&env, problems, subs, constraint);

    let content = solved
        .inner()
        .get_content_without_compacting(expr_var)
        .clone();

    (content, solved.into_inner())
}

/// Used in the with_larger_debug_stack() function, for tests that otherwise
/// run out of stack space in debug builds (but don't in --release builds)
#[allow(dead_code)]
const EXPANDED_STACK_SIZE: usize = 4 * 1024 * 1024;

/// Without this, some tests pass in `cargo test --release` but fail without
/// the --release flag because they run out of stack space. This increases
/// stack size for debug builds only, while leaving the stack space at the default
/// amount for release builds.
#[allow(dead_code)]
#[cfg(debug_assertions)]
pub fn with_larger_debug_stack<F>(run_test: F)
where
    F: FnOnce(),
    F: Send,
    F: 'static,
{
    std::thread::Builder::new()
        .stack_size(EXPANDED_STACK_SIZE)
        .spawn(run_test)
        .expect("Error while spawning expanded dev stack size thread")
        .join()
        .expect("Error while joining expanded dev stack size thread")
}

/// In --release builds, don't increase the stack size. Run the test normally.
/// This way, we find out if any of our tests are blowing the stack even after
/// optimizations in release builds.
#[allow(dead_code)]
#[cfg(not(debug_assertions))]
#[inline(always)]
pub fn with_larger_debug_stack<F>(run_test: F)
where
    F: FnOnce() -> (),
    F: Send,
    F: 'static,
{
    run_test()
}

#[allow(dead_code)]
pub fn can_expr<'a>(arena: &'a Bump, expr_str: &'a str) -> Result<CanExprOut, ParseErrOut<'a>> {
    can_expr_with(arena, test_home(), expr_str)
}

pub struct CanExprOut {
    pub loc_expr: Loc<Expr>,
    pub output: Output,
    pub problems: Vec<Problem>,
    pub home: ModuleId,
    pub interns: Interns,
    pub var_store: VarStore,
    pub var: Variable,
    pub constraint: Constraint,
}

#[derive(Debug)]
pub struct ParseErrOut<'a> {
    pub fail: SourceError<'a, SyntaxError<'a>>,
    pub home: ModuleId,
    pub interns: Interns,
}

#[allow(dead_code)]
pub fn can_expr_with<'a>(
    arena: &'a Bump,
    home: ModuleId,
    expr_str: &'a str,
) -> Result<CanExprOut, ParseErrOut<'a>> {
    let loc_expr = match roc_parse::test_helpers::parse_loc_with(arena, expr_str) {
        Ok(e) => e,
        Err(fail) => {
            let interns = Interns::default();

            return Err(ParseErrOut {
                fail,
                interns,
                home,
            });
        }
    };

    let mut var_store = VarStore::default();
    let var = var_store.fresh();
    let expected = Expected::NoExpectation(Type::Variable(var));
    let mut module_ids = ModuleIds::default();

    // ensure the Test module is accessible in our tests
    module_ids.get_or_insert(&"Test".into());

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
        loc_expr.region,
        &loc_expr.value,
    );

    let constraint = constrain_expr(
        &roc_constrain::expr::Env {
            rigids: ImMap::default(),
            home,
        },
        loc_expr.region,
        &loc_expr.value,
        expected,
    );

    let types = roc_builtins::std::types();

    let imports: Vec<_> = types
        .into_iter()
        .map(|(symbol, (solved_type, region))| Import {
            loc_symbol: Loc::at(region, symbol),
            solved_type,
        })
        .collect();

    //load builtin values
    let (_introduced_rigids, constraint) =
        constrain_imported_values(imports, constraint, &mut var_store);

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

    Ok(CanExprOut {
        loc_expr,
        output,
        problems: env.problems,
        home: env.home,
        var_store,
        interns,
        var,
        constraint,
    })
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

#[allow(dead_code)]
pub fn im_map_from_pairs<K, V, I>(pairs: I) -> ImMap<K, V>
where
    I: IntoIterator<Item = (K, V)>,
    K: Hash + Eq + Clone,
    V: Clone,
{
    let mut answer = ImMap::default();

    for (key, value) in pairs {
        answer.insert(key, value);
    }

    answer
}

#[allow(dead_code)]
pub fn send_set_from<V, I>(elems: I) -> SendSet<V>
where
    I: IntoIterator<Item = V>,
    V: Hash + Eq + Clone,
{
    let mut answer = SendSet::default();

    for elem in elems {
        answer.insert(elem);
    }

    answer
}

#[allow(dead_code)]
pub fn fixtures_dir<'a>() -> PathBuf {
    Path::new("tests").join("fixtures").join("build")
}

#[allow(dead_code)]
pub fn builtins_dir<'a>() -> PathBuf {
    PathBuf::new().join("builtins")
}
