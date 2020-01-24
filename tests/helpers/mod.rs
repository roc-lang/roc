extern crate bumpalo;

use self::bumpalo::Bump;
use roc::can::env::Env;
use roc::can::expr::Output;
use roc::can::expr::{canonicalize_expr, Expr};
use roc::can::operator;
use roc::can::problem::Problem;
use roc::can::scope::Scope;
use roc::collections::{ImMap, ImSet, MutMap, SendSet};
use roc::constrain::expr::constrain_expr;
use roc::ident::Ident;
use roc::module::symbol::Symbol;
use roc::parse;
use roc::parse::ast::{self, Attempting};
use roc::parse::blankspace::space0_before;
use roc::parse::parser::{loc, Fail, Parser, State};
use roc::region::{Located, Region};
use roc::subs::{Subs, VarStore, Variable};
use roc::types::{Constraint, Expected, Type};
use std::hash::Hash;
use std::path::{Path, PathBuf};

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

#[allow(dead_code)]
pub fn parse_with<'a>(arena: &'a Bump, input: &'a str) -> Result<ast::Expr<'a>, Fail> {
    parse_loc_with(arena, input).map(|loc_expr| loc_expr.value)
}

#[allow(dead_code)]
pub fn parse_loc_with<'a>(arena: &'a Bump, input: &'a str) -> Result<Located<ast::Expr<'a>>, Fail> {
    let state = State::new(&input, Attempting::Module);
    let parser = space0_before(loc(parse::expr(0)), 0);
    let answer = parser.parse(&arena, state);

    answer
        .map(|(loc_expr, _)| loc_expr)
        .map_err(|(fail, _)| fail)
}

#[allow(dead_code)]
pub fn can_expr(expr_str: &str) -> (Expr, Output, Vec<Problem>, VarStore, Variable, Constraint) {
    let (loc_expr, output, problems, var_store, var, constraint) =
        can_expr_with(&Bump::new(), "blah", expr_str);

    (loc_expr.value, output, problems, var_store, var, constraint)
}

#[allow(dead_code)]
pub fn uniq_expr(expr_str: &str) -> (Output, Vec<Problem>, Subs, Variable, Constraint) {
    uniq_expr_with(&Bump::new(), expr_str, &ImMap::default())
}

#[allow(dead_code)]
pub fn uniq_expr_with(
    arena: &Bump,
    expr_str: &str,
    declared_idents: &ImMap<Ident, (Symbol, Region)>,
) -> (Output, Vec<Problem>, Subs, Variable, Constraint) {
    let home = "Test";
    let (loc_expr, output, problems, var_store1, variable, _) =
        can_expr_with(arena, home, expr_str);

    // double check
    let var_store2 = VarStore::new(var_store1.fresh());

    let expected2 = Expected::NoExpectation(Type::Variable(variable));
    let constraint2 = roc::uniqueness::constrain_declaration(
        home.into(),
        &var_store2,
        Region::zero(),
        loc_expr,
        declared_idents,
        expected2,
    );

    let subs2 = Subs::new(var_store2.into());

    (output, problems, subs2, variable, constraint2)
}

#[allow(dead_code)]
pub fn can_expr_with(
    arena: &Bump,
    name: &str,
    expr_str: &str,
    declared_idents: &ImMap<Ident, (Symbol, Region)>,
) -> (
    Located<Expr>,
    Output,
    Vec<Problem>,
    VarStore,
    Variable,
    Constraint,
) {
    let loc_expr = parse_loc_with(&arena, expr_str).unwrap_or_else(|e| {
        panic!(
            "can_expr_with() got a parse error when attempting to canonicalize:\n\n{:?} {:?}",
            expr_str, e
        )
    });

    let var_store = VarStore::default();
    let variable = var_store.fresh();
    let expected = Expected::NoExpectation(Type::Variable(variable));
    let module_ids = ModuleIds::default();
    let home = ModuleIds::get_or_insert("Test");

    // Desugar operators (convert them to Apply calls, taking into account
    // operator precedence and associativity rules), before doing other canonicalization.
    //
    // If we did this *during* canonicalization, then each time we
    // visited a BinOp node we'd recursively try to apply this to each of its nested
    // operators, and then again on *their* nested operators, ultimately applying the
    // rules multiple times unnecessarily.
    let loc_expr = operator::desugar_expr(arena, &loc_expr);

    // If we're canonicalizing the declaration `foo = ...` inside the `Main` module,
    // scope_prefix will be "Main.foo$" and its first closure will be named "Main.foo$0"
    let mut scope = Scope::new(home);
    let mut env = Env::new(home.into());
    let (loc_expr, output) = canonicalize_expr(
        &mut env,
        &var_store,
        &mut scope,
        Region::zero(),
        &loc_expr.value,
    );

    let constraint = constrain_expr(
        &roc::constrain::expr::Env {
            rigids: ImMap::default(),
            module_name: home.into(),
        },
        loc_expr.region,
        &loc_expr.value,
        expected,
    );

    (
        loc_expr,
        output,
        env.problems,
        var_store,
        variable,
        constraint,
    )
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
// flex_vars or rigid_vars fields of LetConstraint. These collections should match: no duplicates
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

        assert_eq!(0, 1);
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

    used.remove(&Variable::unsafe_debug_variable(1));

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
        Eq(tipe, expectation, _) => {
            for v in tipe.variables() {
                used.insert(v);
            }

            for v in expectation.get_type_ref().variables() {
                used.insert(v);
            }
        }
        Lookup(_, _, expectation, _) => {
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
