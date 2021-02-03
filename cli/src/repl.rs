use const_format::concatcp;
use gen::{gen_and_eval, ReplOutput};
use roc_gen::llvm::build::OptLevel;
use roc_parse::parser::Bag;
use rustyline::error::ReadlineError;
use rustyline::validate::{self, ValidationContext, ValidationResult, Validator};
use rustyline::Editor;
use rustyline_derive::{Completer, Helper, Highlighter, Hinter};
use std::io;
use target_lexicon::Triple;

const BLUE: &str = "\u{001b}[36m";
const PINK: &str = "\u{001b}[35m";
const END_COL: &str = "\u{001b}[0m";

pub const WELCOME_MESSAGE: &str = concatcp!(
    "\n  The rockin’ ",
    BLUE,
    "roc repl",
    END_COL,
    "\n",
    PINK,
    "────────────────────────",
    END_COL,
    "\n\n"
);
pub const INSTRUCTIONS: &str = "Enter an expression, or :help, or :exit/:q.\n";
pub const PROMPT: &str = concatcp!("\n", BLUE, "»", END_COL, " ");

mod eval;
mod gen;

#[derive(Completer, Helper, Hinter, Highlighter)]
struct ReplHelper {
    validator: InputValidator,
    pending_src: String,
}

impl ReplHelper {
    pub(crate) fn new() -> ReplHelper {
        ReplHelper {
            validator: InputValidator::new(),
            pending_src: String::new(),
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

struct InputValidator {}

impl InputValidator {
    pub(crate) fn new() -> InputValidator {
        InputValidator {}
    }
}

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        if ctx.input().is_empty() {
            Ok(ValidationResult::Incomplete)
        } else {
            Ok(ValidationResult::Valid(None))
        }
    }
}

pub fn main() -> io::Result<()> {
    // To debug rustyline:
    // <UNCOMMENT> env_logger::init();
    // <RUN WITH:> RUST_LOG=rustyline=debug cargo run repl 2> debug.log
    print!("{}{}", WELCOME_MESSAGE, INSTRUCTIONS);

    let mut prev_line_blank = false;
    let mut editor = Editor::<ReplHelper>::new();
    let repl_helper = ReplHelper::new();
    editor.set_helper(Some(repl_helper));

    loop {
        let readline = editor.readline(PROMPT);

        match readline {
            Ok(line) => {
                let trim_line = line.trim();
                editor.add_history_entry(trim_line);

                let pending_src = &mut editor
                    .helper_mut()
                    .expect("Editor helper was not set")
                    .pending_src;

                match trim_line.to_lowercase().as_str() {
                    "" => {
                        if pending_src.is_empty() {
                            print!("\n{}", INSTRUCTIONS);
                        } else if prev_line_blank {
                            // After two blank lines in a row, give up and try parsing it
                            // even though it's going to fail. This way you don't get stuck.
                            match eval_and_format(pending_src.as_str()) {
                                Ok(output) => {
                                    println!("{}", output);
                                }
                                Err(fail) => {
                                    report_parse_error(fail);
                                }
                            }

                            pending_src.clear();
                        } else {
                            pending_src.push('\n');

                            prev_line_blank = true;
                            continue; // Skip the part where we reset prev_line_blank to false
                        }
                    }
                    ":help" => {
                        println!("Use :exit or :q to exit.");
                    }
                    ":exit" => {
                        break;
                    }
                    ":q" => {
                        break;
                    }
                    _ => {
                        let result = if pending_src.is_empty() {
                            eval_and_format(trim_line)
                        } else {
                            pending_src.push('\n');
                            pending_src.push_str(trim_line);

                            eval_and_format(pending_src.as_str())
                        };

                        match result {
                            Ok(output) => {
                                println!("{}", output);
                                pending_src.clear();
                            }
                            //                            Err(Fail {
                            //                                reason: FailReason::Eof(_),
                            //                                ..
                            //                            }) => {}
                            Err(fail) => {
                                report_parse_error(fail);
                                pending_src.clear();
                            }
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                // If we hit an eof, and we're allowed to keep going,
                // append the str to the src we're building up and continue.
                // (We only need to append it here if it was empty before;
                // otherwise, we already appended it before calling eval_and_format.)
                let pending_src = &mut editor
                    .helper_mut()
                    .expect("Editor helper was not set")
                    .pending_src;

                if pending_src.is_empty() {
                    pending_src.push_str("");
                }
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }

        prev_line_blank = false;
    }

    Ok(())
}

fn report_parse_error<'a>(fail: Bag<'a>) {
    println!("TODO Gracefully report parse error in repl: {:?}", fail);
}

fn eval_and_format<'a>(src: &str) -> Result<String, Bag<'a>> {
    gen_and_eval(src.as_bytes(), Triple::host(), OptLevel::Normal).map(|output| match output {
        ReplOutput::NoProblems { expr, expr_type } => {
            format!("\n{} {}:{} {}", expr, PINK, END_COL, expr_type)
        }
        ReplOutput::Problems(lines) => format!("\n{}\n", lines.join("\n\n")),
    })
}
