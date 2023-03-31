use std::path::PathBuf;

use lazy_static::lazy_static;
use regex::Regex;
use roc_can::traverse::{find_ability_member_and_owning_type_at, find_type_at};
use roc_load::LoadedModule;
use roc_module::symbol::{Interns, ModuleId};
use roc_packaging::cache::RocCacheDir;
use roc_problem::can::Problem;
use roc_region::all::{LineColumn, LineColumnRegion, LineInfo, Region};
use roc_reporting::report::{can_problem, type_problem, RocDocAllocator};
use roc_solve_problem::TypeError;
use roc_types::pretty_print::{name_and_print_var, DebugPrint};

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
    F: FnOnce(),
    F: Send,
    F: 'static,
{
    run_test()
}

fn promote_expr_to_module(src: &str) -> String {
    let mut buffer = String::from(indoc!(
        r#"
        app "test"
            imports []
            provides [main] to "./platform"

        main =
        "#
    ));

    for line in src.lines() {
        // indent the body!
        buffer.push_str("    ");
        buffer.push_str(line);
        buffer.push('\n');
    }

    buffer
}

pub fn run_load_and_infer(src: &str) -> Result<(LoadedModule, String), std::io::Error> {
    use bumpalo::Bump;
    use tempfile::tempdir;

    let arena = &Bump::new();

    let module_src;
    let temp;
    if src.starts_with("app") {
        // this is already a module
        module_src = src;
    } else {
        // this is an expression, promote it to a module
        temp = promote_expr_to_module(src);
        module_src = &temp;
    }

    let loaded = {
        let dir = tempdir()?;
        let filename = PathBuf::from("Test.roc");
        let file_path = dir.path().join(filename);
        let result = roc_load::load_and_typecheck_str(
            arena,
            file_path,
            module_src,
            dir.path().to_path_buf(),
            roc_target::TargetInfo::default_x86_64(),
            roc_reporting::report::RenderTarget::Generic,
            RocCacheDir::Disallowed,
            roc_reporting::report::DEFAULT_PALETTE,
        );

        dir.close()?;

        result
    };

    let loaded = loaded.expect("failed to load module");
    Ok((loaded, module_src.to_string()))
}

pub fn format_problems(
    src: &str,
    home: ModuleId,
    interns: &Interns,
    can_problems: Vec<Problem>,
    type_problems: Vec<TypeError>,
) -> (String, String) {
    let filename = PathBuf::from("test.roc");
    let src_lines: Vec<&str> = src.split('\n').collect();
    let lines = LineInfo::new(src);
    let alloc = RocDocAllocator::new(&src_lines, home, interns);

    let mut can_reports = vec![];
    let mut type_reports = vec![];

    for problem in can_problems {
        let report = can_problem(&alloc, &lines, filename.clone(), problem.clone());
        can_reports.push(report.pretty(&alloc));
    }

    for problem in type_problems {
        if let Some(report) = type_problem(&alloc, &lines, filename.clone(), problem.clone()) {
            type_reports.push(report.pretty(&alloc));
        }
    }

    let mut can_reports_buf = String::new();
    let mut type_reports_buf = String::new();
    use roc_reporting::report::CiWrite;
    alloc
        .stack(can_reports)
        .1
        .render_raw(70, &mut CiWrite::new(&mut can_reports_buf))
        .unwrap();
    alloc
        .stack(type_reports)
        .1
        .render_raw(70, &mut CiWrite::new(&mut type_reports_buf))
        .unwrap();

    (can_reports_buf, type_reports_buf)
}

lazy_static! {
    /// Queries of the form
    ///
    /// ```
    /// ^^^{(directive),*}?
    ///
    /// directive :=
    ///   -\d+   # shift the query left by N columns
    ///   inst   # instantiate the given generic instance
    /// ```
    static ref RE_TYPE_QUERY: Regex =
        Regex::new(r#"(?P<where>\^+)(?:\{-(?P<sub>\d+)\})?"#).unwrap();
}

#[derive(Debug, Clone, Copy)]
struct TypeQuery(Region);

/// Parse inference queries in a Roc program.
/// See [RE_TYPE_QUERY].
fn parse_queries(src: &str) -> Vec<TypeQuery> {
    let line_info = LineInfo::new(src);
    let mut queries = vec![];
    let mut consecutive_query_lines = 0;
    for (i, line) in src.lines().enumerate() {
        let mut queries_on_line = RE_TYPE_QUERY.captures_iter(line).into_iter().peekable();

        if queries_on_line.peek().is_none() {
            consecutive_query_lines = 0;
            continue;
        } else {
            consecutive_query_lines += 1;
        }

        for capture in queries_on_line {
            let wher = capture.name("where").unwrap();
            let subtract_col = capture
                .name("sub")
                .and_then(|m| str::parse(m.as_str()).ok())
                .unwrap_or(0);

            let (start, end) = (wher.start() as u32, wher.end() as u32);
            let (start, end) = (start - subtract_col, end - subtract_col);

            let last_line = i as u32 - consecutive_query_lines;
            let start_lc = LineColumn {
                line: last_line,
                column: start,
            };
            let end_lc = LineColumn {
                line: last_line,
                column: end,
            };
            let lc_region = LineColumnRegion::new(start_lc, end_lc);
            let region = line_info.convert_line_column_region(lc_region);

            queries.push(TypeQuery(region));
        }
    }
    queries
}

#[derive(Default)]
pub struct InferOptions {
    pub print_can_decls: bool,
    pub print_only_under_alias: bool,
    pub allow_errors: bool,
}

pub fn infer_queries_help(src: &str, expected: impl FnOnce(&str), options: InferOptions) {
    let (
        LoadedModule {
            module_id: home,
            mut can_problems,
            mut type_problems,
            mut declarations_by_id,
            mut solved,
            interns,
            abilities_store,
            ..
        },
        src,
    ) = run_load_and_infer(src).unwrap();

    let decls = declarations_by_id.remove(&home).unwrap();
    let subs = solved.inner_mut();

    let can_problems = can_problems.remove(&home).unwrap_or_default();
    let type_problems = type_problems.remove(&home).unwrap_or_default();

    let (can_problems, type_problems) =
        format_problems(&src, home, &interns, can_problems, type_problems);

    if !options.allow_errors {
        assert!(
            can_problems.is_empty(),
            "Canonicalization problems: {}",
            can_problems
        );
        assert!(type_problems.is_empty(), "Type problems: {}", type_problems);
    }

    let queries = parse_queries(&src);
    assert!(!queries.is_empty(), "No queries provided!");

    let mut output_parts = Vec::with_capacity(queries.len() + 2);

    if options.print_can_decls {
        use roc_can::debug::{pretty_print_declarations, PPCtx};
        let ctx = PPCtx {
            home,
            interns: &interns,
            print_lambda_names: true,
        };
        let pretty_decls = pretty_print_declarations(&ctx, &decls);
        output_parts.push(pretty_decls);
        output_parts.push("\n".to_owned());
    }

    for TypeQuery(region) in queries.into_iter() {
        let start = region.start().offset;
        let end = region.end().offset;
        let text = &src[start as usize..end as usize];
        let var = find_type_at(region, &decls)
            .unwrap_or_else(|| panic!("No type for {:?} ({:?})!", &text, region));

        let snapshot = subs.snapshot();
        let actual_str = name_and_print_var(
            var,
            subs,
            home,
            &interns,
            DebugPrint {
                print_lambda_sets: true,
                print_only_under_alias: options.print_only_under_alias,
                ignore_polarity: true,
                print_weakened_vars: true,
            },
        );
        subs.rollback_to(snapshot);

        let elaborated =
            match find_ability_member_and_owning_type_at(region, &decls, &abilities_store) {
                Some((spec_type, spec_symbol)) => {
                    format!(
                        "{}#{}({}) : {}",
                        spec_type.as_str(&interns),
                        text,
                        spec_symbol.ident_id().index(),
                        actual_str
                    )
                }
                None => {
                    format!("{} : {}", text, actual_str)
                }
            };

        output_parts.push(elaborated);
    }

    let pretty_output = output_parts.join("\n");

    expected(&pretty_output);
}
