use bumpalo::Bump;
use roc_load::LoadedModule;
use roc_solve::FunctionKind;
use roc_specialize_types::{
    DebugInfo, Env, Interns, MonoCache, MonoExpr, MonoExprs, MonoTypes, RecordFieldIds,
    TupleElemIds,
};
use test_compile::{trim_and_deindent, SpecializedExprOut};
use test_solve_helpers::{format_problems, run_load_and_infer};

fn specialize_expr<'a>(
    arena: &'a Bump,
    src: &str,
    string_interns: &mut Interns<'a>,
) -> SpecializedExprOut {
    let (
        LoadedModule {
            module_id: home,
            mut declarations_by_id,
            mut can_problems,
            mut type_problems,
            interns,
            mut solved,
            mut exposed_to_host,
            abilities_store,
            ..
        },
        src,
    ) = run_load_and_infer(
        trim_and_deindent(&arena, src),
        [],
        false,
        FunctionKind::LambdaSet,
    )
    .unwrap();

    let mut can_problems = can_problems.remove(&home).unwrap_or_default();
    let type_problems = type_problems.remove(&home).unwrap_or_default();

    // Disregard UnusedDef problems, because those are unavoidable when
    // returning a function from the test expression.
    can_problems.retain(|prob| {
        !matches!(
            prob,
            roc_problem::can::Problem::UnusedDef(_, _)
                | roc_problem::can::Problem::UnusedBranchDef(..)
        )
    });

    let (can_problems, type_problems) =
        format_problems(&src, home, &interns, can_problems, type_problems);

    assert_eq!(can_problems, String::new());
    assert_eq!(type_problems, String::new());

    exposed_to_host.retain(|s, _| !abilities_store.is_specialization_name(*s));

    let mut problems = Vec::new();
    let mut debug_info: Option<DebugInfo> = None;
    let mut types_cache = MonoCache::from_solved_subs(&solved);
    let mut mono_types = MonoTypes::new();
    let mut mono_exprs = MonoExprs::new();

    let mut env = Env::new(
        &arena,
        &mut solved,
        &mut types_cache,
        &mut mono_types,
        &mut mono_exprs,
        RecordFieldIds::default(),
        TupleElemIds::default(),
        string_interns,
        &mut debug_info,
        &mut problems,
    );

    let mut home_decls = declarations_by_id.remove(&home).unwrap();
    let main_expr = home_decls.expressions.pop().unwrap().value;

    // This should be our only expr
    assert_eq!(0, home_decls.expressions.len());

    let mono_expr_id = env.to_mono_expr(main_expr);

    SpecializedExprOut {
        mono_expr_id,
        problems,
        mono_types,
        mono_exprs,
    }
}

#[track_caller]
pub fn expect_no_expr(input: impl AsRef<str>) {
    let arena = Bump::new();
    let mut interns = Interns::new();
    let out = specialize_expr(&arena, input.as_ref(), &mut interns);
    let actual = out.mono_expr_id.map(|id| out.mono_exprs.get(id));

    assert_eq!(None, actual, "This input expr should have specialized to being dicarded as zero-sized, but it didn't: {:?}", input.as_ref());
}

#[track_caller]
pub fn expect_mono_expr(input: impl AsRef<str>, mono_expr: MonoExpr) {
    expect_mono_expr_with_interns(|_, _| {}, input, |_| mono_expr);
}

#[track_caller]
pub fn expect_mono_expr_with_interns<T>(
    from_interns: impl for<'a> FnOnce(&'a Bump, &Interns<'a>) -> T,
    input: impl AsRef<str>,
    to_mono_expr: impl FnOnce(T) -> MonoExpr,
) {
    let arena = Bump::new();
    let mut string_interns = Interns::new();
    let out = specialize_expr(&arena, input.as_ref(), &mut string_interns);
    let mono_expr_id = out
            .mono_expr_id
            .expect("This input expr should not have been discarded as zero-sized, but it was discarded: {input:?}");

    let actual_expr = out.mono_exprs.get(mono_expr_id); // Must run first, to populate string interns!

    let expected_expr = to_mono_expr(from_interns(&arena, &string_interns));

    assert_eq!(&expected_expr, actual_expr);
}
