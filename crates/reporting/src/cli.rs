use std::path::PathBuf;

use roc_collections::MutMap;
use roc_module::symbol::{Interns, ModuleId};
use roc_region::all::LineInfo;
use roc_solve_problem::TypeError;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Problems {
    pub errors: usize,
    pub warnings: usize,
}

impl Problems {
    pub fn exit_code(&self) -> i32 {
        // 0 means no problems, 1 means errors, 2 means warnings
        if self.errors > 0 {
            1
        } else {
            self.warnings.min(1) as i32
        }
    }
}

pub fn report_problems(
    total_problems: usize,
    sources: &MutMap<ModuleId, (PathBuf, Box<str>)>,
    interns: &Interns,
    can_problems: &mut MutMap<ModuleId, Vec<roc_problem::can::Problem>>,
    type_problems: &mut MutMap<ModuleId, Vec<TypeError>>,
) -> Problems {
    use crate::report::{
        can_problem, type_problem, Report, RocDocAllocator, Severity::*, DEFAULT_PALETTE,
    };
    let palette = DEFAULT_PALETTE;

    // This will often over-allocate total memory, but it means we definitely
    // never need to re-allocate either the warnings or the errors vec!
    let mut warnings = Vec::with_capacity(total_problems);
    let mut errors = Vec::with_capacity(total_problems);

    for (home, (module_path, src)) in sources.iter() {
        let mut src_lines: Vec<&str> = Vec::new();

        src_lines.extend(src.split('\n'));

        let lines = LineInfo::new(&src_lines.join("\n"));

        // Report parsing and canonicalization problems
        let alloc = RocDocAllocator::new(&src_lines, *home, interns);

        let problems = can_problems.remove(home).unwrap_or_default();

        for problem in problems.into_iter() {
            let report = can_problem(&alloc, &lines, module_path.clone(), problem);
            let severity = report.severity;
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            match severity {
                Warning => {
                    warnings.push(buf);
                }
                RuntimeError => {
                    errors.push(buf);
                }
            }
        }

        let problems = type_problems.remove(home).unwrap_or_default();

        for problem in problems {
            if let Some(report) = type_problem(&alloc, &lines, module_path.clone(), problem) {
                let severity = report.severity;
                let mut buf = String::new();

                report.render_color_terminal(&mut buf, &alloc, &palette);

                match severity {
                    Warning => {
                        warnings.push(buf);
                    }
                    RuntimeError => {
                        errors.push(buf);
                    }
                }
            }
        }
    }

    let problems_reported;

    // Only print warnings if there are no errors
    if errors.is_empty() {
        problems_reported = warnings.len();

        for warning in warnings.iter() {
            println!("\n{}\n", warning);
        }
    } else {
        problems_reported = errors.len();

        for error in errors.iter() {
            println!("\n{}\n", error);
        }
    }

    // If we printed any problems, print a horizontal rule at the end,
    // and then clear any ANSI escape codes (e.g. colors) we've used.
    //
    // The horizontal rule is nice when running the program right after
    // compiling it, as it lets you clearly see where the compiler
    // errors/warnings end and the program output begins.
    if problems_reported > 0 {
        println!("{}\u{001B}[0m\n", Report::horizontal_rule(&palette));
    }

    Problems {
        errors: errors.len(),
        warnings: warnings.len(),
    }
}
