extern crate roc;

use std::fs::File;
use std::io::prelude::*;
use roc::expr::Expr::*;
use roc::expr::Expr;
use roc::eval::{eval, from_evaluated};
use roc::parse;
use std::io;

fn main() -> std::io::Result<()> {
    let mut file = File::open("test.roc")?;
    let mut contents = String::new();

    file.read_to_string(&mut contents)?;

    let expr = parse::parse_string(contents.as_str()).unwrap();

    eval_task(expr)
}

fn eval_task(expr: Expr) -> std::io::Result<()> {
    match from_evaluated(eval(expr)) {
        Error(problem) => {
            println!("\n\u{001B}[4mruntime error\u{001B}[24m\n\n{:?}\n", problem);

            Ok(())
        },
        ApplyVariant(name, Some(mut exprs)) => {
            match name.as_str() {
                "Echo" => {
                    let payload = exprs.pop().unwrap();
                    let next_expr = exprs.pop().unwrap();

                    println!("{}", payload);

                    eval_task(Apply(Box::new(next_expr), vec![EmptyRecord]))
                },
                "Read" => {
                    let mut input = String::new();
                    io::stdin().read_line(&mut input)?;

                    println!("[debug] You said: {}", input);

                    Ok(())
                },
                _ => {
                    display_expr(ApplyVariant(name, Some(exprs)));

                    Ok(())
                }
            }
        },
        output => {
            display_expr(output);

            Ok(())
        }
    }
}

fn display_expr(expr: Expr) {
    println!("\n\u{001B}[4mroc out\u{001B}[24m\n\n{}\n", expr);
}
