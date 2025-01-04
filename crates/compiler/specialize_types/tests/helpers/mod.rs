// TODO [mono2]: re-enable when ready
#![allow(dead_code)]

use bumpalo::Bump;
use core::fmt::Write;
use roc_load::LoadedModule;
use roc_region::all::Region;
use roc_solve::FunctionKind;
use roc_specialize_types::{
    DebugInfo, Env, Interns, MonoExpr, MonoExprs, MonoPattern, MonoPatterns, MonoTypeCache,
    MonoTypes, RecordFieldIds, TupleElemIds, WhenBranches,
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
        trim_and_deindent(arena, src),
        [],
        false,
        FunctionKind::LambdaSet,
    )
    .unwrap();

    let mut can_problems = can_problems.remove(&home).unwrap_or_default();

    // Disregard UnusedDef problems, because those are unavoidable when
    // returning a function from the test expression.
    can_problems.retain(|prob| {
        !matches!(
            prob,
            roc_problem::can::Problem::UnusedDef(_, _)
                | roc_problem::can::Problem::UnusedBranchDef(..)
        )
    });

    let mut type_problems = type_problems.remove(&home).unwrap_or_default();

    // Disregard Redundant pattern errors so we can test pattern specialization directly
    type_problems.retain(|prob| {
        !matches!(
            prob,
            roc_solve_problem::TypeError::Exhaustive(roc_exhaustive::Error::Redundant { .. })
        )
    });

    let (can_problems, type_problems) =
        format_problems(&src, home, &interns, can_problems, type_problems);

    assert_eq!(can_problems, String::new());
    assert_eq!(type_problems, String::new());

    exposed_to_host.retain(|s, _| !abilities_store.is_specialization_name(*s));

    let mut problems = Vec::new();
    let mut debug_info: Option<DebugInfo> = None;
    let mut types_cache = MonoTypeCache::from_solved_subs(&solved);
    let mut mono_types = MonoTypes::new();
    let mut mono_exprs = MonoExprs::new();
    let mut mono_patterns = MonoPatterns::new();
    let mut when_branches = WhenBranches::new();

    let mut env = Env::new(
        arena,
        &mut solved,
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

    let mut home_decls = declarations_by_id.remove(&home).unwrap();
    let main_expr = home_decls.expressions.pop().unwrap().value;

    // This should be our only expr
    assert_eq!(0, home_decls.expressions.len());

    let region = Region::zero();
    let mono_expr = env.to_mono_expr(&main_expr);
    let mono_expr_id = mono_exprs.add(mono_expr, region);

    SpecializedExprOut {
        mono_expr_id,
        problems,
        mono_types,
        mono_exprs,
        mono_patterns,
        when_branches,
        region,
    }
}

#[track_caller]
pub fn expect_unit(input: impl AsRef<str>) {
    let arena = Bump::new();
    let mut interns = Interns::new();
    let out = specialize_expr(&arena, input.as_ref(), &mut interns);
    let actual = out.mono_exprs.get_expr(out.mono_expr_id);

    assert_eq!(MonoExpr::Unit, *actual, "This input expr should have specialized to being dicarded as zero-sized, but it didn't: {:?}", input.as_ref());
}

#[track_caller]
pub fn expect_mono_expr(input: impl AsRef<str>, mono_expr: MonoExpr) {
    expect_mono_expr_with_interns(input, |_, _| mono_expr);
}

#[track_caller]
pub fn expect_mono_expr_str(input: impl AsRef<str>, expr_str: impl AsRef<str>) {
    expect_mono_expr_custom(
        input,
        |_, _, _, _, _| expr_str.as_ref().to_string(),
        |arena, mono_exprs, mono_patterns, when_branches, interns, expr| {
            dbg_mono_expr(
                arena,
                mono_exprs,
                mono_patterns,
                when_branches,
                interns,
                expr,
            )
            .to_string()
        },
    );
}

fn dbg_mono_expr<'a>(
    arena: &'a Bump,
    mono_exprs: &MonoExprs,
    mono_patterns: &MonoPatterns,
    when_branches: &WhenBranches,
    interns: &Interns<'a>,
    expr: &MonoExpr,
) -> &'a str {
    let mut buf = bumpalo::collections::String::new_in(arena);

    dbg_mono_expr_help(
        arena,
        mono_exprs,
        mono_patterns,
        when_branches,
        interns,
        expr,
        &mut buf,
    )
    .unwrap();

    buf.into_bump_str()
}

fn dbg_mono_expr_help<'a>(
    arena: &'a Bump,
    mono_exprs: &MonoExprs,
    mono_patterns: &MonoPatterns,
    when_branches: &WhenBranches,
    interns: &Interns<'a>,
    expr: &MonoExpr,
    buf: &mut impl Write,
) -> Result<(), core::fmt::Error> {
    match expr {
        MonoExpr::Str(interned_str_id) => {
            write!(buf, "Str({:?})", interns.get_str(arena, *interned_str_id))
        }
        MonoExpr::Number(number) => {
            write!(buf, "Number({:?})", number)
        }
        MonoExpr::Struct(field_exprs) => {
            write!(buf, "Struct([")?;

            for (index, expr) in mono_exprs.iter_slice(field_exprs.as_slice()).enumerate() {
                if index > 0 {
                    write!(buf, ", ")?;
                }

                dbg_mono_expr_help(
                    arena,
                    mono_exprs,
                    mono_patterns,
                    when_branches,
                    interns,
                    expr,
                    buf,
                )?;
            }

            write!(buf, "])")
        }
        MonoExpr::Unit => {
            write!(buf, "{{}}")
        }
        MonoExpr::If {
            branch_type: _,
            branches,
            final_else,
        } => {
            write!(buf, "If(",)?;

            for (index, (cond, branch)) in mono_exprs.iter_pair_slice(*branches).enumerate() {
                if index > 0 {
                    write!(buf, ", ")?;
                }

                dbg_mono_expr_help(
                    arena,
                    mono_exprs,
                    mono_patterns,
                    when_branches,
                    interns,
                    cond,
                    buf,
                )?;
                write!(buf, " -> ")?;
                dbg_mono_expr_help(
                    arena,
                    mono_exprs,
                    mono_patterns,
                    when_branches,
                    interns,
                    branch,
                    buf,
                )?;
                write!(buf, ")")?;
            }

            write!(buf, ", ")?;
            dbg_mono_expr_help(
                arena,
                mono_exprs,
                mono_patterns,
                when_branches,
                interns,
                mono_exprs.get_expr(*final_else),
                buf,
            )?;
            write!(buf, ")")
        }
        MonoExpr::Lookup(ident, _mono_type_id) => {
            write!(buf, "{:?}", ident)
        }
        MonoExpr::When {
            value,
            value_type: _,
            branch_type: _,
            branches,
        } => {
            write!(buf, "When(")?;

            dbg_mono_expr_help(
                arena,
                mono_exprs,
                mono_patterns,
                when_branches,
                interns,
                mono_exprs.get_expr(*value),
                buf,
            )?;

            write!(buf, ", ")?;

            for (index, branch) in when_branches.iter_slice(branches.as_slice()).enumerate() {
                if index > 0 {
                    write!(buf, ", ")?;
                }

                let branch = unsafe { branch.assume_init() };

                for (index, pattern) in mono_patterns
                    .iter_slice(branch.patterns.as_slice())
                    .enumerate()
                {
                    if index > 0 {
                        write!(buf, " | ")?;
                    }

                    dbg_mono_pattern_help(arena, mono_patterns, interns, pattern, buf)?;
                }

                if let Some(guard) = branch.guard {
                    write!(buf, " if ")?;

                    dbg_mono_expr_help(
                        arena,
                        mono_exprs,
                        mono_patterns,
                        when_branches,
                        interns,
                        mono_exprs.get_expr(guard),
                        buf,
                    )?;
                }

                write!(buf, " -> ")?;

                dbg_mono_expr_help(
                    arena,
                    mono_exprs,
                    mono_patterns,
                    when_branches,
                    interns,
                    mono_exprs.get_expr(branch.value),
                    buf,
                )?;
            }

            write!(buf, ")")
        }
        // MonoExpr::List { elem_type, elems } => todo!(),
        // MonoExpr::ParameterizedLookup {
        //     name,
        //     lookup_type,
        //     params_name,
        //     params_type,
        // } => todo!(),
        // MonoExpr::When {
        //     cond,
        //     cond_type,
        //     branch_type,
        //     branches,
        // } => todo!(),
        // MonoExpr::If {
        //     branch_type,
        //     branches,
        //     final_else,
        // } => todo!(),
        // MonoExpr::LetRec { defs, ending_expr } => todo!(),
        // MonoExpr::LetNonRec { def, ending_expr } => todo!(),
        // MonoExpr::Call {
        //     fn_type,
        //     fn_expr,
        //     args,
        //     closure_type,
        // } => todo!(),
        // MonoExpr::RunLowLevel { op, args, ret_type } => todo!(),
        // MonoExpr::ForeignCall {
        //     foreign_symbol,
        //     args,
        //     ret_type,
        // } => todo!(),
        // MonoExpr::Lambda {
        //     fn_type,
        //     arguments,
        //     body,
        //     captured_symbols,
        //     recursive,
        // } => todo!(),
        // MonoExpr::Crash { msg, expr_type } => todo!(),
        // MonoExpr::StructAccess {
        //     record_expr,
        //     record_type,
        //     field_type,
        //     field_id,
        // } => todo!(),
        // MonoExpr::RecordUpdate {
        //     record_type,
        //     record_name,
        //     updates,
        // } => todo!(),
        // MonoExpr::SmallTag {
        //     discriminant,
        //     tag_union_type,
        //     args,
        // } => todo!(),
        // MonoExpr::BigTag {
        //     discriminant,
        //     tag_union_type,
        //     args,
        // } => todo!(),
        // MonoExpr::Expect {
        //     condition,
        //     continuation,
        //     lookups_in_cond,
        // } => todo!(),
        // MonoExpr::Dbg {
        //     source_location,
        //     source,
        //     msg,
        //     continuation,
        //     expr_type,
        //     name,
        // } => todo!(),
        MonoExpr::CompilerBug(problem) => {
            write!(buf, "CompilerBug({:?})", problem)
        }
        other => {
            todo!("Implement dbg_mono_expr for {:?}", other)
        }
    }
}

fn dbg_mono_pattern<'a>(
    arena: &'a Bump,
    mono_patterns: &MonoPatterns,
    interns: &Interns<'a>,
    pattern: &MonoPattern,
) -> &'a str {
    let mut buf = bumpalo::collections::String::new_in(arena);

    dbg_mono_pattern_help(arena, mono_patterns, interns, pattern, &mut buf).unwrap();

    buf.into_bump_str()
}

fn dbg_mono_pattern_help<'a>(
    _arena: &'a Bump,
    _mono_patterns: &MonoPatterns,
    _interns: &Interns<'a>,
    pattern: &MonoPattern,
    buf: &mut impl Write,
) -> Result<(), core::fmt::Error> {
    match pattern {
        MonoPattern::Identifier(ident) => {
            write!(buf, "Identifier({:?})", ident)
        }
        MonoPattern::As(_, _) => {
            todo!()
        }
        MonoPattern::StrLiteral(_) => {
            todo!()
        }
        MonoPattern::NumberLiteral(number) => {
            write!(buf, "Number({:?})", number)
        }
        MonoPattern::AppliedTag { .. } => {
            todo!()
        }
        MonoPattern::StructDestructure { .. } => {
            todo!()
        }
        MonoPattern::List { .. } => {
            todo!()
        }
        MonoPattern::Underscore => {
            write!(buf, "Underscore")
        }
        MonoPattern::CompilerBug(problem) => {
            write!(buf, "CompilerBug({:?}", problem)
        }
    }
}

#[track_caller]
pub fn expect_mono_expr_with_interns(
    input: impl AsRef<str>,
    from_interns: impl for<'a> Fn(&'a Bump, &Interns<'a>) -> MonoExpr,
) {
    expect_mono_expr_custom(
        input,
        |arena, _exprs, _, _, interns| from_interns(arena, interns),
        |_, _, _, _, _, expr| *expr,
    );
}

#[track_caller]
pub fn expect_mono_expr_custom<T: PartialEq + core::fmt::Debug>(
    input: impl AsRef<str>,
    to_expected: impl for<'a> Fn(&'a Bump, &MonoExprs, &MonoPatterns, &WhenBranches, &Interns<'a>) -> T,
    to_actual: impl for<'a> Fn(
        &'a Bump,
        &MonoExprs,
        &MonoPatterns,
        &WhenBranches,
        &Interns<'a>,
        &MonoExpr,
    ) -> T,
) {
    let arena = Bump::new();
    let mut string_interns = Interns::new();
    let out = specialize_expr(&arena, input.as_ref(), &mut string_interns);

    let actual_expr = out.mono_exprs.get_expr(out.mono_expr_id); // Must run first, to populate string interns!
    let actual = to_actual(
        &arena,
        &out.mono_exprs,
        &out.mono_patterns,
        &out.when_branches,
        &string_interns,
        actual_expr,
    );
    let expected = to_expected(
        &arena,
        &out.mono_exprs,
        &out.mono_patterns,
        &out.when_branches,
        &string_interns,
    );

    assert_eq!(expected, actual);
}

#[track_caller]
pub fn expect_mono_pattern_str(input: impl AsRef<str>, expr_str: impl AsRef<str>) {
    expect_mono_pattern_custom(
        input,
        |_, _, _| expr_str.as_ref().to_string(),
        |arena, mono_patterns, interns, pattern| {
            dbg_mono_pattern(arena, mono_patterns, interns, pattern).to_string()
        },
    );
}

#[track_caller]
pub fn expect_mono_pattern_custom<T: PartialEq + core::fmt::Debug>(
    input: impl AsRef<str>,
    to_expected: impl for<'a> Fn(&'a Bump, &MonoPatterns, &Interns<'a>) -> T,
    to_actual: impl for<'a> Fn(&'a Bump, &MonoPatterns, &Interns<'a>, &MonoPattern) -> T,
) {
    let arena = Bump::new();
    let mut string_interns = Interns::new();
    let input = input.as_ref();
    let src = format!(
        r#"
            when crash("") is
                {input} -> crash("")
                _ -> crash("")
        "#
    );
    let out = specialize_expr(&arena, &src, &mut string_interns);

    let expr = out.mono_exprs.get_expr(out.mono_expr_id);

    let MonoExpr::When { branches, .. } = expr else {
        panic!("Expected a when expression, but found {:?}", expr);
    };

    let branch = unsafe {
        out.when_branches
            .iter_slice(branches.as_slice())
            .next()
            .unwrap()
            .assume_init()
    };

    let actual_pattern = out
        .mono_patterns
        .iter_slice(branch.patterns.as_slice())
        .next()
        .unwrap();

    let actual = to_actual(&arena, &out.mono_patterns, &string_interns, actual_pattern);

    let expected = to_expected(&arena, &out.mono_patterns, &string_interns);

    assert_eq!(expected, actual);
}
