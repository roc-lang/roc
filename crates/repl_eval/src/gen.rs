use bumpalo::Bump;
use roc_load::{ExecutionMode, LoadConfig, Threading};
use roc_reporting::report::Palette;
use std::path::PathBuf;

use roc_fmt::annotation::Formattable;
use roc_fmt::annotation::{Newlines, Parens};
use roc_load::{LoadingProblem, MonomorphizedModule};
use roc_parse::ast::Expr;
use roc_region::all::LineInfo;
use roc_reporting::report::{can_problem, type_problem, RocDocAllocator};
use roc_target::TargetInfo;

use crate::eval::ToAstProblem;

pub enum ReplOutput {
    Problems(Vec<String>),
    NoProblems {
        expr: String,
        expr_type: String,
        val_name: String,
    },
}

pub fn format_answer(
    arena: &Bump,
    res_answer: Result<Expr, ToAstProblem>,
    expr_type_str: String,
    val_name: String,
) -> ReplOutput {
    let mut expr = roc_fmt::Buf::new_in(arena);

    use ToAstProblem::*;
    match res_answer {
        Ok(answer) => {
            answer.format_with_options(&mut expr, Parens::NotNeeded, Newlines::Yes, 0);
        }
        Err(FunctionLayout) => {
            expr.indent(0);
            expr.push_str("<function>");
        }
    }

    ReplOutput::NoProblems {
        expr: expr.into_bump_str().to_string(),
        expr_type: expr_type_str,
        val_name,
    }
}

pub fn compile_to_mono<'a>(
    arena: &'a Bump,
    src: &str,
    target_info: TargetInfo,
    palette: Palette,
) -> Result<MonomorphizedModule<'a>, Vec<String>> {
    let filename = PathBuf::from("");
    let src_dir = PathBuf::from("fake/test/path");

    let module_src = arena.alloc(promote_expr_to_module(src));

    let exposed_types = Default::default();
    let loaded = roc_load::load_and_monomorphize_from_str(
        arena,
        filename,
        module_src,
        src_dir,
        exposed_types,
        LoadConfig {
            target_info,
            render: roc_reporting::report::RenderTarget::ColorTerminal,
            threading: Threading::Single,
            exec_mode: ExecutionMode::Executable,
        },
    );

    let mut loaded = match loaded {
        Ok(v) => v,
        Err(LoadingProblem::FormattedReport(report)) => {
            return Err(vec![report]);
        }
        Err(e) => {
            panic!("error while loading module: {:?}", e)
        }
    };

    let MonomorphizedModule {
        interns,
        sources,
        can_problems,
        type_problems,
        ..
    } = &mut loaded;

    let mut lines = Vec::new();

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
            let report = can_problem(&alloc, &line_info, module_path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            lines.push(buf);
        }

        for problem in type_probs {
            if let Some(report) = type_problem(&alloc, &line_info, module_path.clone(), problem) {
                let mut buf = String::new();

                report.render_color_terminal(&mut buf, &alloc, &palette);

                lines.push(buf);
            }
        }
    }

    if !lines.is_empty() {
        Err(lines)
    } else {
        Ok(loaded)
    }
}

fn promote_expr_to_module(src: &str) -> String {
    let mut buffer =
        String::from("app \"app\" provides [replOutput] to \"./platform\"\n\nreplOutput =\n");

    for line in src.lines() {
        // indent the body!
        buffer.push_str("    ");
        buffer.push_str(line);
        buffer.push('\n');
    }

    buffer
}
