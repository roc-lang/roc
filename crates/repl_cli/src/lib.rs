//! Command Line Interface (CLI) functionality for the Read-Evaluate-Print-Loop (REPL).
mod cli_gen;

use bumpalo::Bump;
use const_format::concatcp;
use roc_load::MonomorphizedModule;
use roc_mono::ir::OptLevel;
use roc_repl_eval::gen::Problems;
use roc_repl_ui::colors::{CYAN, END_COL};
use roc_repl_ui::repl_state::{ReplAction, ReplState};
use roc_repl_ui::{format_output, is_incomplete, CONT_PROMPT, PROMPT, SHORT_INSTRUCTIONS, TIPS};
use roc_reporting::report::{
    strip_colors, to_file_problem_report_string, ANSI_STYLE_CODES, DEFAULT_PALETTE,
};
use roc_target::Target;
use rustyline::highlight::{Highlighter, PromptInfo};
use rustyline::validate::{self, ValidationContext, ValidationResult, Validator};
use rustyline::Config;
use rustyline_derive::{Completer, Helper, Hinter};
use std::borrow::Cow;
use target_lexicon::Triple;

use crate::cli_gen::eval_llvm;

pub const WELCOME_MESSAGE: &str = concatcp!(
    "\n  The rockin' ",
    CYAN,
    "roc repl\n",
    "────────────────────────",
    END_COL,
    "\n\n"
);

#[derive(Completer, Helper, Hinter, Default)]
pub struct ReplHelper {
    validator: InputValidator,
    state: ReplState,
}

pub fn main(has_color: bool, has_header: bool) -> i32 {
    use rustyline::error::ReadlineError;
    use rustyline::Editor;

    let strip_colors_if_necessary = |s: &str| {
        if has_color {
            s.to_string()
        } else {
            strip_colors(s)
        }
    };

    // To debug rustyline:
    // <UNCOMMENT> env_logger::init();
    // <RUN WITH:> RUST_LOG=rustyline=debug cargo run repl 2> debug.log
    if has_header {
        print!(
            "{}{}",
            strip_colors_if_necessary(WELCOME_MESSAGE),
            strip_colors_if_necessary(SHORT_INSTRUCTIONS),
        );
    }

    let editor_color_mode = if has_color {
        rustyline::ColorMode::Enabled
    } else {
        rustyline::ColorMode::Disabled
    };
    let mut editor =
        Editor::<ReplHelper>::with_config(Config::builder().color_mode(editor_color_mode).build());
    let repl_helper = ReplHelper::default();
    editor.set_helper(Some(repl_helper));
    let target = Triple::host().into();
    let mut arena = Bump::new();

    loop {
        match editor.readline(&strip_colors_if_necessary(PROMPT)) {
            Ok(line) => {
                let line = line.trim();

                editor.add_history_entry(line);

                let repl_state = &mut editor
                    .helper_mut()
                    .expect("Editor helper was not set")
                    .state;

                arena.reset();
                match repl_state.step(&arena, line, target, DEFAULT_PALETTE) {
                    ReplAction::Eval { opt_mono, problems } => {
                        let output = evaluate(opt_mono, problems, target);
                        // If there was no output, don't print a blank line!
                        // (This happens for something like a type annotation.)
                        if !output.is_empty() {
                            println!("{}", strip_colors_if_necessary(&output));
                        }
                    }
                    ReplAction::Exit => {
                        return 0;
                    }
                    ReplAction::FileProblem { filename, error } => {
                        println!(
                            "{}",
                            to_file_problem_report_string(filename, error, has_color)
                        );
                    }
                    ReplAction::Help => {
                        println!("{TIPS}");
                    }
                    ReplAction::Nothing => {}
                }
            }
            #[cfg(windows)]
            Err(ReadlineError::WindowResize) => {
                // This is fine; just ignore it.
            }
            Err(ReadlineError::Eof) => {
                // End of input; we're done!
                return 0;
            }
            Err(ReadlineError::Interrupted) => {
                // Ctrl-C was pressed
                eprintln!("CTRL-C");
            }
            Err(err) => {
                eprintln!("REPL error: {err:?}");
                return 1;
            }
        }
    }
}

pub fn evaluate(
    opt_mono: Option<MonomorphizedModule<'_>>,
    problems: Problems,
    target: Target,
) -> String {
    let opt_output = opt_mono.and_then(|mono| eval_llvm(mono, target, OptLevel::Normal));
    format_output(ANSI_STYLE_CODES, opt_output, problems)
}

#[derive(Default)]
struct InputValidator {}

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        if is_incomplete(ctx.input()) {
            Ok(ValidationResult::Incomplete)
        } else {
            Ok(ValidationResult::Valid(None))
        }
    }
}

impl Highlighter for ReplHelper {
    fn has_continuation_prompt(&self) -> bool {
        true
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        info: PromptInfo<'_>,
    ) -> Cow<'b, str> {
        if info.line_no() > 0 {
            CONT_PROMPT.into()
        } else {
            prompt.into()
        }
    }
}

impl Validator for ReplHelper {
    fn validate(
        &self,
        ctx: &mut validate::ValidationContext,
    ) -> rustyline::Result<validate::ValidationResult> {
        self.validator.validate(ctx)
    }

    fn validate_while_typing(&self) -> bool {
        self.validator.validate_while_typing()
    }
}
