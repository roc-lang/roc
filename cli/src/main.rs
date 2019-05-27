extern crate roc;

use std::fs::File;
use std::io::prelude::*;
use roc::eval::eval;
use roc::parse;

fn main() -> std::io::Result<()> {
    let mut file = File::open("test.roc")?;
    let mut contents = String::new();

    file.read_to_string(&mut contents)?;

    let expr = parse::parse_string(contents.as_str()).unwrap();

    println!("\n\u{001B}[4mroc out\u{001B}[24m\n\n{}\n", eval(expr).to_string());

    Ok(())
}
