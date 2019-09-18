extern crate roc;

use roc::eval::Evaluated::*;
use roc::eval::{call, eval, Evaluated};
use roc::expr::Expr;
use roc::parse;
use roc::region::{Located, Region};
use std::fs::File;
use std::io;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let argv = std::env::args().into_iter().collect::<Vec<String>>();

    match argv.get(1) {
        Some(filename) => {
            let mut file = File::open(filename)?;
            let mut contents = String::new();

            file.read_to_string(&mut contents)?;

            let expr = parse::parse_string(contents.as_str()).unwrap();

            process_task(eval(expr))
        }
        None => {
            println!("Usage: roc FILENAME.roc");

            Ok(())
        }
    }
}

fn process_task(evaluated: Evaluated) -> std::io::Result<()> {
    match evaluated {
        EvalError(region, problem) => {
            println!(
                "\n\u{001B}[4mruntime error\u{001B}[24m\n\n{} at {}\n",
                format!("{}", problem),
                format!("line {}, column {}", region.start_line, region.start_col)
            );

            Ok(())
        }
        ApplyVariant(name, Some(mut vals)) => {
            match name.as_str() {
                "Echo" => {
                    // Extract the string from the Echo variant.
                    let string_to_be_displayed = match vals.pop() {
                        Some(Str(payload)) => payload,
                        Some(EvalError(region, err)) => {
                            panic!(
                                "RUNTIME ERROR in Echo: {} at {}",
                                format!("{}", err),
                                format!("line {}, column {}", region.start_line, region.start_col)
                            );
                        }
                        Some(val) => {
                            panic!("TYPE MISMATCH in Echo: {}", format!("{}", val));
                        }
                        None => {
                            panic!("TYPE MISMATCH in Echo: None");
                        }
                    };

                    // Print the string to the console, since that's what Echo does!
                    println!("{}", string_to_be_displayed);

                    // Continue with the callback.
                    let callback = vals.pop().unwrap();

                    process_task(call(
                        Region {
                            start_line: 0,
                            start_col: 0,
                            end_line: 0,
                            end_col: 0,
                        },
                        callback,
                        vec![with_zero_loc(Expr::EmptyRecord)],
                    ))
                }
                "Read" => {
                    // Read a line from from stdin, since that's what Read does!
                    let mut input = String::new();

                    io::stdin().read_line(&mut input)?;

                    // Continue with the callback.
                    let callback = vals.pop().unwrap();

                    process_task(call(
                        Region {
                            start_line: 0,
                            start_col: 0,
                            end_line: 0,
                            end_col: 0,
                        },
                        callback,
                        vec![with_zero_loc(Expr::Str(input.trim().to_string()))],
                    ))
                }
                "Success" => {
                    // We finished all our tasks. Great! No need to print anything.
                    Ok(())
                }
                _ => {
                    // We don't recognize this variant, so display it and exit.
                    display_val(ApplyVariant(name, Some(vals)));

                    Ok(())
                }
            }
        }
        output => {
            // We don't recognize this value, so display it and exit.
            display_val(output);

            Ok(())
        }
    }
}

fn with_zero_loc<T>(val: T) -> Located<T> {
    Located::new(
        val,
        Region {
            start_line: 0,
            start_col: 0,

            end_line: 0,
            end_col: 0,
        },
    )
}

fn display_val(evaluated: Evaluated) {
    println!("\n\u{001B}[4mroc out\u{001B}[24m\n\n{}\n", evaluated);
}
