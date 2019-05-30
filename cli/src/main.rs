extern crate roc;

use std::fs::File;
use std::io::prelude::*;
use roc::expr::Expr::*;
use roc::expr::Expr;
use roc::eval::eval;
use roc::eval::from_evaluated;
use roc::parse;
use std::io;

fn main() -> std::io::Result<()> {
    let mut file = File::open("test.roc")?;
    let mut contents = String::new();

    file.read_to_string(&mut contents)?;

    let expr = parse::parse_string(contents.as_str()).unwrap();

    match from_evaluated(eval(expr)) {
        Error(problem) => {
            println!("\n\u{001B}[4mruntime error\u{001B}[24m\n\n{:?}\n", problem)
        },
        ApplyVariant(name, payload) => {
            match name.as_str() {
                "Echo" => {
                    println!("{}", payload.unwrap().first().unwrap());
                },
                "Read" => {
                    let mut input = String::new();
                    io::stdin().read_line(&mut input)?;

                    println!("[debug] You said: {}", input);
                },
                _ => {
                    display_expr(ApplyVariant(name, payload));
                }
            }
        },
        output => {
            display_expr(output);
        }
    };

    Ok(())
}

fn display_expr(expr: Expr) {
    println!("\n\u{001B}[4mroc out\u{001B}[24m\n\n{}\n", expr);
}
