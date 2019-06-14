extern crate roc;

use std::fs::File;
use std::io::prelude::*;
use roc::expr::Expr;
use roc::eval::{Evaluated, eval, call};
use roc::eval::Evaluated::*;
use roc::parse;
use std::io;

fn main() -> std::io::Result<()> {
    let mut file = File::open("test.roc")?;
    let mut contents = String::new();

    file.read_to_string(&mut contents)?;

    let expr = parse::parse_string(contents.as_str()).unwrap();

    process_task(eval(expr))
}

fn process_task(evaluated: Evaluated) -> std::io::Result<()> {
    match evaluated {
        EvalError(problem) => {
            println!("\n\u{001B}[4mruntime error\u{001B}[24m\n\n{:?}\n", problem);

            Ok(())
        },
        ApplyVariant(name, Some(mut vals)) => {
            match name.as_str() {
                "Echo" => {
                    let payload = vals.pop().unwrap();
                    let callback = vals.pop().unwrap();

                    println!("{}", payload);

                    process_task(call(callback, vec![Expr::EmptyRecord]))
                },
                "Read" => {
                    let callback = vals.pop().unwrap();
                    let mut input = String::new();

                    io::stdin().read_line(&mut input)?;

                    process_task(call(callback, vec![Expr::Str(input)]))
                },
                _ => {
                    display_val(ApplyVariant(name, Some(vals)));

                    Ok(())
                }
            }
        },
        output => {
            display_val(output);

            Ok(())
        }
    }
}

fn display_val(evaluated: Evaluated) {
    println!("\n\u{001B}[4mroc out\u{001B}[24m\n\n{}\n", evaluated);
}
