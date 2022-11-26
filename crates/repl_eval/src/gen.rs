use bumpalo::Bump;
use roc_load::{ExecutionMode, LoadConfig, Threading};
use roc_packaging::cache::{self, RocCacheDir};
use roc_reporting::report::{Palette, Severity};
use std::path::PathBuf;

use roc_fmt::annotation::Formattable;
use roc_fmt::annotation::{Newlines, Parens};
use roc_load::{LoadingProblem, MonomorphizedModule};
use roc_parse::ast::Expr;
use roc_region::all::LineInfo;
use roc_reporting::report::{can_problem, type_problem, RocDocAllocator};
use roc_target::TargetInfo;

#[derive(Debug)]
pub struct ReplOutput {
    pub expr: String,
    pub expr_type: String,
}

pub fn format_answer<'a>(arena: &'a Bump, answer: Expr<'_>) -> &'a str {
    match answer {
        Expr::Closure(_, _) | Expr::MalformedClosure => "<function>",
        _ => {
            let mut expr = roc_fmt::Buf::new_in(arena);

            answer.format_with_options(&mut expr, Parens::NotNeeded, Newlines::Yes, 0);

            expr.into_bump_str()
        }
    }
}

#[derive(Default, Debug)]
pub struct Problems {
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}

impl Problems {
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty() && self.warnings.is_empty()
    }
}

pub fn compile_to_mono<'a, 'i, I: Iterator<Item = &'i str>>(
    arena: &'a Bump,
    defs: I,
    expr: &str,
    target_info: TargetInfo,
    palette: Palette,
) -> (Option<MonomorphizedModule<'a>>, Problems) {
    let filename = PathBuf::from("");
    let src_dir = PathBuf::from("fake/test/path");
    let (bytes_before_expr, module_src) = promote_expr_to_module(arena, defs, expr);
    let exposed_types = Default::default();
    let loaded = roc_load::load_and_monomorphize_from_str(
        arena,
        filename,
        module_src,
        src_dir,
        exposed_types,
        RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
        LoadConfig {
            target_info,
            render: roc_reporting::report::RenderTarget::ColorTerminal,
            palette,
            threading: Threading::Single,
            exec_mode: ExecutionMode::Executable,
        },
    );

    let mut loaded = match loaded {
        Ok(v) => v,
        Err(LoadingProblem::FormattedReport(report)) => {
            return (
                None,
                Problems {
                    errors: vec![report],
                    warnings: Vec::new(),
                },
            );
        }
        Err(e) => {
            todo!("error while loading module: {:?}", e)
        }
    };

    let MonomorphizedModule {
        interns,
        sources,
        can_problems,
        type_problems,
        ..
    } = &mut loaded;

    let mut problems = Problems::default();

    let errors = &mut problems.errors;
    let warnings = &mut problems.warnings;

    for (home, (module_path, src)) in sources.iter() {
        let can_probs = can_problems.remove(home).unwrap_or_default();
        let type_probs = type_problems.remove(home).unwrap_or_default();

        let error_count = can_probs.len() + type_probs.len();

        if error_count == 0 {
            continue;
        }

        let line_info = LineInfo::new(module_src);
        let src_lines: Vec<&str> = src.split('\n').collect();

        // Report parsing and canonicalization problems
        let alloc = RocDocAllocator::new(&src_lines, *home, interns);

        for problem in can_probs.into_iter() {
            // Filter out all warnings and errors whose regions end before this,
            // because they must be part of the defs (excluding the most renently added def,
            // if that's the one being evaluated) and therefore not things we should show.
            // This filters out things like shadowing warnings and unused def warnings.
            if problem.region().unwrap_or_default().end().offset as usize >= bytes_before_expr {
                let report = can_problem(&alloc, &line_info, module_path.clone(), problem);
                let severity = report.severity;
                let mut buf = String::new();

                report.render_color_terminal(&mut buf, &alloc, &palette);

                match severity {
                    Severity::Warning => {
                        warnings.push(buf);
                    }
                    Severity::RuntimeError => {
                        errors.push(buf);
                    }
                }
            }
        }

        for problem in type_probs {
            if let Some(report) = type_problem(&alloc, &line_info, module_path.clone(), problem) {
                let severity = report.severity;
                let mut buf = String::new();

                report.render_color_terminal(&mut buf, &alloc, &palette);

                match severity {
                    Severity::Warning => {
                        warnings.push(buf);
                    }
                    Severity::RuntimeError => {
                        errors.push(buf);
                    }
                }
            }
        }
    }

    (Some(loaded), problems)
}

fn promote_expr_to_module<'a, 'i, I: Iterator<Item = &'i str>>(
    arena: &'a Bump,
    defs: I,
    expr: &str,
) -> (usize, &'a str) {
    const REPL_MODULE_HEADER: &str = "app \"app\" provides [replOutput] to \"./platform\"\n\n";
    const REPL_MODULE_MAIN_DEF: &str = "replOutput =\n";
    const INDENT: &str = "    ";

    let mut buffer = bumpalo::collections::string::String::from_str_in(REPL_MODULE_HEADER, arena);

    for line in defs {
        // don't indent the defs
        buffer.push_str(line);
        buffer.push_str("\n\n");
    }

    buffer.push_str(REPL_MODULE_MAIN_DEF);

    let bytes_before_expr = buffer.len();

    for line in expr.lines() {
        // indent the expr!
        buffer.push_str(INDENT);
        buffer.push_str(line);
        buffer.push('\n');
    }

    (bytes_before_expr, buffer.into_bump_str())
}
