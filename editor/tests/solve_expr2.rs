#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

use bumpalo::Bump;
use roc_can::expected::Expected;
use roc_collections::all::MutMap;
use roc_editor::lang::solve;
use roc_editor::lang::{
    constrain::constrain_expr,
    constrain::Constraint,
    expr::{str_to_expr2, Env},
    pool::Pool,
    scope::Scope,
    types::Type2,
};
use roc_module::ident::Lowercase;
use roc_module::symbol::Interns;
use roc_module::symbol::Symbol;
use roc_module::symbol::{IdentIds, ModuleIds};
use roc_region::all::Region;
use roc_types::solved_types::Solved;
use roc_types::subs::{Subs, Variable};
use roc_types::{pretty_print::content_to_string, subs::VarStore};

fn run_solve<'a>(
    arena: &'a Bump,
    mempool: &mut Pool,
    aliases: MutMap<Symbol, roc_types::types::Alias>,
    rigid_variables: MutMap<Variable, Lowercase>,
    constraint: Constraint,
    var_store: VarStore,
) -> (Solved<Subs>, solve::Env, Vec<solve::TypeError>) {
    let env = solve::Env {
        vars_by_symbol: MutMap::default(),
        aliases,
    };

    let mut subs = Subs::new(var_store);

    for (var, name) in rigid_variables {
        subs.rigid_var(var, name);
    }

    // Now that the module is parsed, canonicalized, and constrained,
    // we need to type check it.
    let mut problems = Vec::new();

    // Run the solver to populate Subs.
    let (solved_subs, solved_env) =
        solve::run(arena, mempool, &env, &mut problems, subs, &constraint);

    (solved_subs, solved_env, problems)
}

fn infer_eq(actual: &str, expected_str: &str) {
    let mut env_pool = Pool::with_capacity(1024);
    let env_arena = Bump::new();
    let code_arena = Bump::new();

    let mut var_store = VarStore::default();
    let var = var_store.fresh();
    let dep_idents = IdentIds::exposed_builtins(8);
    let exposed_ident_ids = IdentIds::default();
    let mut module_ids = ModuleIds::default();
    let mod_id = module_ids.get_or_insert(&"ModId123".into());

    let mut env = Env::new(
        mod_id,
        &env_arena,
        &mut env_pool,
        &mut var_store,
        dep_idents,
        &module_ids,
        exposed_ident_ids,
    );

    let mut scope = Scope::new(env.home, env.pool, env.var_store);

    let region = Region::zero();

    let expr2_result = str_to_expr2(&code_arena, actual, &mut env, &mut scope, region);

    match expr2_result {
        Ok((expr, _)) => {
            let constraint = constrain_expr(
                &code_arena,
                &mut env,
                &expr,
                Expected::NoExpectation(Type2::Variable(var)),
                Region::zero(),
            );

            let Env {
                pool,
                var_store: ref_var_store,
                mut dep_idents,
                ..
            } = env;

            // extract the var_store out of the env again
            let mut var_store = VarStore::default();
            std::mem::swap(ref_var_store, &mut var_store);

            let (mut solved, _, _) = run_solve(
                &code_arena,
                pool,
                Default::default(),
                Default::default(),
                constraint,
                var_store,
            );

            let subs = solved.inner_mut();

            let content = subs.get_content_without_compacting(var);

            // Connect the ModuleId to it's IdentIds
            dep_idents.insert(mod_id, env.ident_ids);

            let interns = Interns {
                module_ids: env.module_ids.clone(),
                all_ident_ids: dep_idents,
            };

            let actual_str = content_to_string(content, subs, mod_id, &interns);

            assert_eq!(actual_str, expected_str);
        }
        Err(e) => panic!("syntax error {:?}", e),
    }
}

#[test]
fn constrain_str() {
    infer_eq(
        indoc!(
            r#"
            "type inference!"
            "#
        ),
        "Str",
    )
}

// This will be more useful once we actually map
// strings less than 15 chars to SmallStr
#[test]
fn constrain_small_str() {
    infer_eq(
        indoc!(
            r#"
            "a"
            "#
        ),
        "Str",
    )
}

#[test]
fn constrain_empty_record() {
    infer_eq(
        indoc!(
            r#"
            {}
            "#
        ),
        "{}",
    )
}

#[test]
fn constrain_small_int() {
    infer_eq(
        indoc!(
            r#"
            12
            "#
        ),
        "Num *",
    )
}

#[test]
fn constrain_float() {
    infer_eq(
        indoc!(
            r#"
            3.14
            "#
        ),
        "Float *",
    )
}

#[test]
fn constrain_record() {
    infer_eq(
        indoc!(
            r#"
            { x : 1, y : "hi" }
            "#
        ),
        "{ x : Num *, y : Str }",
    )
}

#[test]
fn constrain_empty_list() {
    infer_eq(
        indoc!(
            r#"
            []
            "#
        ),
        "List *",
    )
}

#[test]
fn constrain_list() {
    infer_eq(
        indoc!(
            r#"
            [ 1, 2 ]
            "#
        ),
        "List (Num *)",
    )
}

#[test]
fn constrain_list_of_records() {
    infer_eq(
        indoc!(
            r#"
            [ { x: 1 }, { x: 3 } ]
            "#
        ),
        "List { x : Num * }",
    )
}

#[test]
fn constrain_global_tag() {
    infer_eq(
        indoc!(
            r#"
            Foo
            "#
        ),
        "[ Foo ]*",
    )
}

#[test]
fn constrain_private_tag() {
    infer_eq(
        indoc!(
            r#"
            @Foo
            "#
        ),
        "[ @Foo ]*",
    )
}

#[test]
fn constrain_call_and_accessor() {
    infer_eq(
        indoc!(
            r#"
            .foo { foo: "bar" }
            "#
        ),
        "Str",
    )
}

#[test]
fn constrain_access() {
    infer_eq(
        indoc!(
            r#"
            { foo: "bar" }.foo
            "#
        ),
        "Str",
    )
}

#[test]
fn constrain_if() {
    infer_eq(
        indoc!(
            r#"
            if True then Green else Red
            "#
        ),
        "[ Green, Red ]*",
    )
}

#[test]
fn constrain_when() {
    infer_eq(
        indoc!(
            r#"
            when if True then Green else Red is
                Green -> Blue
                Red -> Purple
            "#
        ),
        "[ Blue, Purple ]*",
    )
}

#[test]
fn constrain_let_value() {
    infer_eq(
        indoc!(
            r#"
            person = { name: "roc" }

            person
            "#
        ),
        "{ name : Str }",
    )
}

#[test]
fn constrain_update() {
    infer_eq(
        indoc!(
            r#"
            person = { name: "roc" }

            { person & name: "bird" } 
            "#
        ),
        "{ name : Str }",
    )
}

#[ignore = "TODO: implement builtins in the editor"]
#[test]
fn constrain_run_low_level() {
    infer_eq(
        indoc!(
            r#"
            List.map [ { name: "roc" }, { name: "bird" } ] .name
            "#
        ),
        "List Str",
    )
}

#[test]
fn constrain_closure() {
    infer_eq(
        indoc!(
            r#"
            x = 1

            \{} -> x
            "#
        ),
        "{}* -> Num *",
    )
}
