//! UI functionality, shared between CLI and web, for the Read-Evaluate-Print-Loop (REPL).
// We don't do anything here related to the terminal (doesn't exist on the web) or LLVM (too big for the web).
pub mod colors;
pub mod repl_state;

use bumpalo::Bump;
use colors::{BLUE, END_COL, GREEN, PINK};
use const_format::concatcp;
use repl_state::{parse_src, ParseOutcome};
use roc_parse::ast::{Expr, ValueDef};
use roc_repl_eval::gen::{Problems, ReplOutput};
use roc_reporting::report::StyleCodes;

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

// TODO add link to repl tutorial(does not yet exist).
pub const TIPS: &str = concatcp!(
    "\nEnter an expression to evaluate, or a definition (like ",
    BLUE,
    "x = 1",
    END_COL,
    ") to use in future expressions.\n\nUnless there was a compile-time error, expressions get automatically named so you can refer to them later.\nFor example, if you see ",
    GREEN,
    "# val1",
    END_COL,
    " after an output, you can now refer to that expression as ",
    BLUE,
    "val1",
    END_COL,
    " in future expressions.\n\n",
    "Tips:\n\n",
    if cfg!(target_family = "wasm") {
        // In the web REPL, the :quit command doesn't make sense. Just close the browser tab!
        // We use ctrl-Enter for newlines because it's nicer than our workaround for Unix terminals (see below)
        concatcp!(
            BLUE,
            "  - ",
            END_COL,
            PINK,
            "ctrl-Enter",
            END_COL,
            " makes a newline\n\n",
            BLUE,
            "  - ",
            END_COL,
            ":help"
        )
    } else {
        // We use ctrl-v + ctrl-j for newlines because on Unix, terminals cannot distinguish between ctrl-Enter and Enter
        concatcp!(
            BLUE,
            "  - ",
            END_COL,
            PINK,
            "ctrl-v",
            END_COL,
            " + ",
            PINK,
            "ctrl-j",
            END_COL,
            " makes a newline\n\n",
            BLUE,
            "  - ",
            END_COL,
            ":q to quit\n\n",
            BLUE,
            "  - ",
            END_COL,
            ":help"
        )
    }
);

// For when nothing is entered in the repl
// TODO add link to repl tutorial(does not yet exist).
pub const SHORT_INSTRUCTIONS: &str = "Enter an expression, or :help, or :q to quit.\n\n";
pub const PROMPT: &str = concatcp!(BLUE, "»", END_COL, " ");
pub const CONT_PROMPT: &str = concatcp!(BLUE, "…", END_COL, " ");

pub fn is_incomplete(input: &str) -> bool {
    let arena = Bump::new();

    match parse_src(&arena, input) {
        ParseOutcome::Incomplete => !input.ends_with('\n'),
        // Standalone annotations are default incomplete, because we can't know
        // whether they're about to annotate a body on the next line
        // (or if not, meaning they stay standalone) until you press Enter again!
        //
        // So it's Incomplete until you've pressed Enter again (causing the input to end in "\n")
        ParseOutcome::ValueDef(ValueDef::Annotation(_, _)) if !input.ends_with('\n') => true,
        ParseOutcome::Expr(Expr::When(_, _)) => {
            // There might be lots of `when` branches, so don't assume the user is done entering
            // them until they enter a blank line!
            !input.ends_with('\n')
        }
        ParseOutcome::Empty
        | ParseOutcome::Help
        | ParseOutcome::Exit
        | ParseOutcome::ValueDef(_)
        | ParseOutcome::TypeDef(_)
        | ParseOutcome::SyntaxErr
        | ParseOutcome::Expr(_) => false,
    }
}

pub fn format_output(
    style_codes: StyleCodes,
    opt_output: Option<ReplOutput>,
    problems: Problems,
    opt_var_name: Option<String>,
    dimensions: Option<(usize, usize)>,
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
                buf.push_str(style_codes.magenta); // Color for the type separator
                buf.push_str(EXPR_TYPE_SEPARATOR);
                buf.push_str(style_codes.reset);
                buf.push_str(&expr_type);
            }

            // Print var_name right-aligned on the last line of output.
            if let Some(var_name) = opt_var_name {
                use unicode_segmentation::UnicodeSegmentation;

                const VAR_NAME_PREFIX: &str = " # "; // e.g. in " # val1"
                const VAR_NAME_COLUMN_MAX: usize = 32; // Right-align the var_name at this column

                let term_width = match dimensions {
                    Some((width, _)) => width.min(VAR_NAME_COLUMN_MAX),
                    None => VAR_NAME_COLUMN_MAX,
                };

                let expr_with_type = format!("{expr}{EXPR_TYPE_SEPARATOR}{expr_type}");

                // Count graphemes because we care about what's *rendered* in the terminal
                let last_line_len = expr_with_type
                    .split('\n')
                    .last()
                    .unwrap_or_default()
                    .graphemes(true)
                    .count();
                let var_name_len =
                    var_name.graphemes(true).count() + VAR_NAME_PREFIX.graphemes(true).count();
                let spaces_needed = if last_line_len + var_name_len > term_width {
                    buf.push('\n');
                    term_width - var_name_len
                } else {
                    term_width - last_line_len - var_name_len
                };

                for _ in 0..spaces_needed {
                    buf.push(' ');
                }

                buf.push_str(style_codes.green);
                buf.push_str(VAR_NAME_PREFIX);
                buf.push_str(&var_name);
                buf.push_str(style_codes.reset);
                buf.push('\n');
            }
        }
    }

    buf
}
