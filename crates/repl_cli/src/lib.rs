//! Command Line Interface (CLI) functionality for the Read-Evaluate-Print-Loop (REPL).
mod cli_gen;
mod colors;
pub mod repl_state;

use colors::{BLUE, END_COL, PINK};
use const_format::concatcp;
use repl_state::ReplState;

use crate::repl_state::PROMPT;

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

// For when nothing is entered in the repl
// TODO add link to repl tutorial(does not yet exist).
pub const SHORT_INSTRUCTIONS: &str = "Enter an expression, or :help, or :q to quit.\n\n";

pub fn main() -> i32 {
    use rustyline::error::ReadlineError;
    use rustyline::Editor;

    // To debug rustyline:
    // <UNCOMMENT> env_logger::init();
    // <RUN WITH:> RUST_LOG=rustyline=debug cargo run repl 2> debug.log
    print!("{}{}", WELCOME_MESSAGE, SHORT_INSTRUCTIONS);

    let mut editor = Editor::<ReplState>::new();
    let repl_helper = ReplState::new();
    editor.set_helper(Some(repl_helper));

    loop {
        match editor.readline(PROMPT) {
            Ok(line) => {
                editor.add_history_entry(line.trim());

                let dimensions = editor.dimensions();
                let repl_helper = editor.helper_mut().expect("Editor helper was not set");

                match repl_helper.step(&line, dimensions) {
                    Ok(output) => {
                        // If there was no output, don't print a blank line!
                        // (This happens for something like a type annotation.)
                        if !output.is_empty() {
                            println!("{}", output);
                        }
                    }
                    Err(exit_code) => return exit_code,
                };
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
                eprintln!("CTRL-C");
                return 1;
            }
            Err(err) => {
                eprintln!("REPL error: {:?}", err);
                return 1;
            }
        }
    }
}
