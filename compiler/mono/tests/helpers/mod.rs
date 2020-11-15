extern crate bumpalo;

use self::bumpalo::Bump;
use roc_builtins::unique::uniq_stdlib;
use roc_can::constraint::Constraint;
use roc_can::env::Env;
use roc_can::expected::Expected;
use roc_can::expr::{canonicalize_expr, Expr, Output};
use roc_can::operator;
use roc_can::scope::Scope;
use roc_collections::all::{ImMap, ImSet, MutMap, SendMap, SendSet};
use roc_constrain::expr::constrain_expr;
use roc_constrain::module::{constrain_imported_values, Import};
use roc_module::ident::Ident;
use roc_module::symbol::{IdentIds, Interns, ModuleId, ModuleIds, Symbol};
use roc_parse::ast::{self, Attempting};
use roc_parse::blankspace::space0_before;
use roc_parse::parser::{loc, Fail, Parser, State};
use roc_problem::can::Problem;
use roc_region::all::{Located, Region};
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
    problems: &mut Vec<roc_solve::solve::TypeError>,
    constraint: &Constraint,
    expr_var: Variable,
) -> (Content, Subs) {
    let env = solve::Env {
        aliases: MutMap::default(),
        vars_by_symbol: SendMap::default(),
    };
    let (solved, _) = solve::run(&env, problems, subs, constraint);

    let content = solved.inner().get_without_compacting(expr_var).content;

    (content, solved.into_inner())
}

#[allow(dead_code)]
pub fn parse_with<'a>(arena: &'a Bump, input: &'a str) -> Result<ast::Expr<'a>, Fail> {
    parse_loc_with(arena, input).map(|loc_expr| loc_expr.value)
}

#[allow(dead_code)]
pub fn parse_loc_with<'a>(arena: &'a Bump, input: &'a str) -> Result<Located<ast::Expr<'a>>, Fail> {
    let state = State::new(input.as_bytes(), Attempting::Module);
    let parser = space0_before(loc(roc_parse::expr::expr(0)), 0);
    let answer = parser.parse(&arena, state);

    answer
        .map(|(loc_expr, _)| loc_expr)
        .map_err(|(fail, _)| fail)
}

#[allow(dead_code)]
pub fn can_expr(expr_str: &str) -> CanExprOut {
    can_expr_with(&Bump::new(), test_home(), expr_str)
}

#[allow(dead_code)]
pub fn uniq_expr(
    expr_str: &str,
) -> (
    Located<Expr>,
    Output,
    Vec<Problem>,
    Subs,
    Variable,
    Constraint,
    ModuleId,
    Interns,
) {
    let declared_idents: &ImMap<Ident, (Symbol, Region)> = &ImMap::default();

    uniq_expr_with(&Bump::new(), expr_str, declared_idents)
}

#[allow(dead_code)]
pub fn uniq_expr_with(
    arena: &Bump,
    expr_str: &str,
    declared_idents: &ImMap<Ident, (Symbol, Region)>,
) -> (
    Located<Expr>,
    Output,
    Vec<Problem>,
    Subs,
    Variable,
    Constraint,
    ModuleId,
    Interns,
) {
    let home = test_home();
    let CanExprOut {
        loc_expr,
        output,
        problems,
        var_store: mut old_var_store,
        var,
        interns,
        ..
    } = can_expr_with(arena, home, expr_str);

    // double check
    let mut var_store = VarStore::new(old_var_store.fresh());

    let expected2 = Expected::NoExpectation(Type::Variable(var));
    let constraint = roc_constrain::uniq::constrain_declaration(
        home,
        &mut var_store,
        Region::zero(),
        &loc_expr,
        declared_idents,
        expected2,
    );

    let stdlib = uniq_stdlib();

    let types = stdlib.types;
    let imports: Vec<_> = types
        .into_iter()
        .map(|(symbol, (solved_type, region))| Import {
            loc_symbol: Located::at(region, symbol),
            solved_type,
        })
        .collect();

    // load builtin values

    // TODO what to do with those rigids?
    let (_introduced_rigids, constraint) =
        constrain_imported_values(imports, constraint, &mut var_store);

    let subs2 = Subs::new(var_store.into());

    (
        loc_expr, output, problems, subs2, var, constraint, home, interns,
    )
}

pub struct CanExprOut {
    pub loc_expr: Located<Expr>,
    pub output: Output,
    pub problems: Vec<Problem>,
    pub home: ModuleId,
    pub interns: Interns,
    pub var_store: VarStore,
    pub var: Variable,
    pub constraint: Constraint,
}

#[allow(dead_code)]
pub fn can_expr_with(arena: &Bump, home: ModuleId, expr_str: &str) -> CanExprOut {
    let loc_expr = parse_loc_with(&arena, expr_str).unwrap_or_else(|e| {
        panic!(
            "can_expr_with() got a parse error when attempting to canonicalize:\n\n{:?} {:?}",
            expr_str, e
        )
    });

    let mut var_store = VarStore::default();
    let var = var_store.fresh();
    let expected = Expected::NoExpectation(Type::Variable(var));
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
    let mut env = Env::new(home, dep_idents, &module_ids, IdentIds::default());
    let (loc_expr, output) = canonicalize_expr(
        &mut env,
        &mut var_store,
        &mut scope,
        Region::zero(),
        &loc_expr.value,
    );

    // Add the builtins' defs.
    let mut with_builtins = loc_expr.value;

    // Add builtin defs (e.g. List.get) directly to the canonical Expr,
    // since we aren't using modules here.
    let builtin_defs = roc_can::builtins::builtin_defs(&mut var_store);

    for (symbol, def) in builtin_defs {
        if output.references.lookups.contains(&symbol) || output.references.calls.contains(&symbol)
        {
            with_builtins = roc_can::expr::Expr::LetNonRec(
                Box::new(def),
                Box::new(Located {
                    region: Region::zero(),
                    value: with_builtins,
                }),
                var_store.fresh(),
            );
        }
    }

    let loc_expr = Located {
        region: loc_expr.region,
        value: with_builtins,
    };

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
            loc_symbol: Located::at(region, symbol),
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

    CanExprOut {
        loc_expr,
        output,
        problems: env.problems,
        home: env.home,
        var_store,
        interns,
        var,
        constraint,
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

// Check constraints
//
// Keep track of the used (in types or expectations) variables, and the declared variables (in
// flex_vars or rigid_vars fields of LetConstraint. These roc_collections should match: no duplicates
// and no variables that are used but not declared are allowed.
//
// There is one exception: the initial variable (that stores the type of the whole expression) is
// never declared, but is used.
#[allow(dead_code)]
pub fn assert_correct_variable_usage(constraint: &Constraint) {
    // variables declared in constraint (flex_vars or rigid_vars)
    // and variables actually used in constraints
    let (declared, used) = variable_usage(constraint);

    let used: ImSet<Variable> = used.clone().into();
    let mut decl: ImSet<Variable> = declared.rigid_vars.clone().into();

    for var in declared.flex_vars.clone() {
        decl.insert(var);
    }

    let diff = used.clone().relative_complement(decl);

    // NOTE: this checks whether we're using variables that are not declared. For recursive type
    // definitions,  their rigid types are declared twice, which is correct!
    if !diff.is_empty() {
        println!("VARIABLE USAGE PROBLEM");

        println!("used: {:?}", &used);
        println!("rigids: {:?}", &declared.rigid_vars);
        println!("flexs: {:?}", &declared.flex_vars);

        println!("difference: {:?}", &diff);

        panic!("variable usage problem (see stdout for details)");
    }
}

#[derive(Default)]
pub struct SeenVariables {
    pub rigid_vars: Vec<Variable>,
    pub flex_vars: Vec<Variable>,
}

pub fn variable_usage(con: &Constraint) -> (SeenVariables, Vec<Variable>) {
    let mut declared = SeenVariables::default();
    let mut used = ImSet::default();
    variable_usage_help(con, &mut declared, &mut used);

    used.remove(unsafe { &Variable::unsafe_test_debug_variable(1) });
    used.remove(unsafe { &Variable::unsafe_test_debug_variable(2) });
    used.remove(unsafe { &Variable::unsafe_test_debug_variable(3) });

    let mut used_vec: Vec<Variable> = used.into_iter().collect();
    used_vec.sort();

    declared.rigid_vars.sort();
    declared.flex_vars.sort();

    (declared, used_vec)
}

fn variable_usage_help(con: &Constraint, declared: &mut SeenVariables, used: &mut ImSet<Variable>) {
    use Constraint::*;

    match con {
        True | SaveTheEnvironment => (),
        Eq(tipe, expectation, _, _) => {
            for v in tipe.variables() {
                used.insert(v);
            }

            for v in expectation.get_type_ref().variables() {
                used.insert(v);
            }
        }
        Store(tipe, var, _, _) => {
            for v in tipe.variables() {
                used.insert(v);
            }

            used.insert(*var);
        }
        Lookup(_, expectation, _) => {
            for v in expectation.get_type_ref().variables() {
                used.insert(v);
            }
        }
        Pattern(_, _, tipe, pexpectation) => {
            for v in tipe.variables() {
                used.insert(v);
            }

            for v in pexpectation.get_type_ref().variables() {
                used.insert(v);
            }
        }
        Let(letcon) => {
            declared.rigid_vars.extend(letcon.rigid_vars.clone());
            declared.flex_vars.extend(letcon.flex_vars.clone());

            variable_usage_help(&letcon.defs_constraint, declared, used);
            variable_usage_help(&letcon.ret_constraint, declared, used);
        }
        And(constraints) => {
            for sub in constraints {
                variable_usage_help(sub, declared, used);
            }
        }
    }
}

/// Used in the with_larger_debug_stack() function, for tests that otherwise
/// run out of stack space in debug builds (but don't in --release builds)
#[allow(dead_code)]
const EXPANDED_STACK_SIZE: usize = 8 * 1024 * 1024;

/// Without this, some tests pass in `cargo test --release` but fail without
/// the --release flag because they run out of stack space. This increases
/// stack size for debug builds only, while leaving the stack space at the default
/// amount for release builds.
#[allow(dead_code)]
#[cfg(debug_assertions)]
pub fn with_larger_debug_stack<F>(run_test: F)
where
    F: FnOnce() -> (),
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
