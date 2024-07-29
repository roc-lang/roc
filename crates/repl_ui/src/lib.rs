//! UI functionality, shared between CLI and web, for the Read-Evaluate-Print-Loop (REPL).
// We don't do anything here related to the terminal (doesn't exist on the web) or LLVM (too big for the web).
pub mod colors;
pub mod repl_state;

use bumpalo::Bump;
use colors::{CYAN, END_COL, GREEN};
use const_format::concatcp;
use repl_state::{parse_src, ParseOutcome};
use roc_parse::ast::{Expr, ExtractSpaces, ValueDef};
use roc_repl_eval::gen::{Problems, ReplOutput};
use roc_reporting::report::StyleCodes;

// TODO add link to repl tutorial (does not yet exist).
pub const TIPS: &str = concatcp!(
    "\nEnter an expression to evaluate, or a definition (like ",
    CYAN,
    "x = 1",
    END_COL,
    ") to use later.\n\n",
    if cfg!(target_family = "wasm") {
        "" // In the web repl, we render tips in the UI around the repl instead of in the repl itself.
    } else {
        // We use ctrl-v + ctrl-j for newlines because on Unix, terminals cannot distinguish between Shift-Enter and Enter
        concatcp!(
            CYAN,
            "  - ",
            END_COL,
            GREEN,
            "ctrl-v",
            END_COL,
            " + ",
            GREEN,
            "ctrl-j",
            END_COL,
            " makes a newline\n",
            CYAN,
            "  - ",
            END_COL,
            GREEN,
            ":q",
            END_COL,
            " quits\n",
            CYAN,
            "  - ",
            END_COL,
            GREEN,
            ":help",
            END_COL,
            " shows this text again\n",
        )
    }
);

// For when nothing is entered in the repl
// TODO add link to repl tutorial(does not yet exist).
pub const SHORT_INSTRUCTIONS: &str = "Enter an expression, or :help, or :q to quit.\n\n";
pub const PROMPT: &str = concatcp!(CYAN, "»", END_COL, " ");
pub const CONT_PROMPT: &str = concatcp!(CYAN, "…", END_COL, " ");

pub fn is_incomplete(input: &str) -> bool {
    let arena = Bump::new();

    match parse_src(&arena, input) {
        ParseOutcome::Incomplete => !input.ends_with('\n'),
        ParseOutcome::DefsAndExpr(defs, None) => {
            // Standalone annotations are default incomplete, because we can't know
            // whether they're about to annotate a body on the next line
            // (or if not, meaning they stay standalone) until you press Enter again!
            //
            // So it's Incomplete until you've pressed Enter again (causing the input to end in "\n")
            if matches!(defs.last(), Some(Err(ValueDef::Annotation(_, _)))) {
                !input.ends_with('\n')
            } else {
                false
            }
        }
        ParseOutcome::DefsAndExpr(_, Some(expr)) => {
            if matches!(expr.extract_spaces().item, Expr::When(..)) {
                // There might be lots of `when` branches, so don't assume the user is done entering
                // them until they enter a blank line!
                !input.ends_with('\n')
            } else {
                false
            }
        }
        ParseOutcome::Empty | ParseOutcome::Help | ParseOutcome::Exit | ParseOutcome::SyntaxErr => {
            false
        }
    }
}

pub fn format_output(
    style_codes: StyleCodes,
    opt_output: Option<ReplOutput>,
    problems: Problems,
) -> String {
    let mut buf = String::new();

    for message in problems.errors.iter().chain(problems.warnings.iter()) {
        if !buf.is_empty() {
            buf.push_str("\n\n");
        }

        buf.push('\n');
        buf.push_str(message);
        buf.push('\n');
    }

    if let Some(ReplOutput { expr, expr_type }) = opt_output {
        // If expr was empty, it was a type annotation or ability declaration;
        // don't print anything!
        //
        // Also, for now we also don't print anything if there was a compile-time error.
        // In the future, it would be great to run anyway and print useful output here!
        if !expr.is_empty() && problems.errors.is_empty() {
            const EXPR_TYPE_SEPARATOR: &str = " : "; // e.g. in "5 : Num *"

            // Print the expr and its type
            {
                buf.push('\n');
                buf.push_str(&expr);
                buf.push_str(style_codes.green); // Color for the type separator
                buf.push_str(EXPR_TYPE_SEPARATOR);
                buf.push_str(style_codes.reset);
                buf.push_str(&expr_type);
            }
        }
    }

    buf
}
