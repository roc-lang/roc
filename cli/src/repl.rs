use gen::{gen, ReplOutput};
use roc_gen::llvm::build::OptLevel;
use roc_parse::parser::{Fail, FailReason};
use std::io::{self, Write};
use target_lexicon::Triple;

pub const WELCOME_MESSAGE: &str = "\n  The rockin’ \u{001b}[36mroc repl\u{001b}[0m\n\u{001b}[35m────────────────────────\u{001b}[0m\n\n";
pub const INSTRUCTIONS: &str = "Enter an expression, or :help, or :exit.\n";
pub const PROMPT: &str = "\n\u{001b}[36m»\u{001b}[0m ";
pub const ELLIPSIS: &str = "\u{001b}[36m…\u{001b}[0m ";

mod eval;
mod gen;

pub fn main() -> io::Result<()> {
    use std::io::BufRead;

    print!("{}{}", WELCOME_MESSAGE, INSTRUCTIONS);

    // Loop

    let mut pending_src = String::new();
    let mut prev_line_blank = false;

    loop {
        if pending_src.is_empty() {
            print!("{}", PROMPT);
        } else {
            print!("{}", ELLIPSIS);
        }

        io::stdout().flush().unwrap();

        let stdin = io::stdin();
        let line = stdin
            .lock()
            .lines()
            .next()
            .expect("there was no next line")
            .expect("the line could not be read");

        let line = line.trim();

        match line.to_lowercase().as_str() {
            ":help" => {
                println!("Use :exit to exit.");
            }
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
            ":exit" => {
                break;
            }
            _ => {
                let result = if pending_src.is_empty() {
                    eval_and_format(line)
                } else {
                    pending_src.push('\n');
                    pending_src.push_str(line);

                    eval_and_format(pending_src.as_str())
                };

                match result {
                    Ok(output) => {
                        println!("{}", output);
                        pending_src.clear();
                    }
                    Err(Fail {
                        reason: FailReason::Eof(_),
                        ..
                    }) => {
                        // If we hit an eof, and we're allowed to keep going,
                        // append the str to the src we're building up and continue.
                        // (We only need to append it here if it was empty before;
                        // otherwise, we already appended it before calling print_output.)

                        if pending_src.is_empty() {
                            pending_src.push_str(line);
                        }
                    }
                    Err(fail) => {
                        report_parse_error(fail);
                        pending_src.clear();
                    }
                }
            }
        }

        prev_line_blank = false;
    }

    Ok(())
}

fn report_parse_error(fail: Fail) {
    println!("TODO Gracefully report parse error in repl: {:?}", fail);
}

fn eval_and_format(src: &str) -> Result<String, Fail> {
    gen(src.as_bytes(), Triple::host(), OptLevel::Normal).map(|output| match output {
        ReplOutput::NoProblems { expr, expr_type } => {
            format!("\n{} \u{001b}[35m:\u{001b}[0m {}", expr, expr_type)
        }
        ReplOutput::Problems(lines) => format!("\n{}\n", lines.join("\n\n")),
    })
}
