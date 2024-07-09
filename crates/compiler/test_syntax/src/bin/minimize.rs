use test_syntax::{minimize::print_minimizations, test_helpers::InputKind};

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() != 3 {
        eprintln!("Usage: {} [expr|full|moduledefs|header] <input>", args[0]);
        std::process::exit(1);
    }

    let kind = match args[1].as_str() {
        "expr" => InputKind::Expr,
        "full" => InputKind::Full,
        "moduledefs" => InputKind::ModuleDefs,
        "header" => InputKind::Header,
        _ => {
            eprintln!("Invalid input kind: {}", args[1]);
            std::process::exit(1);
        }
    };

    let text = std::fs::read_to_string(&args[2]).unwrap();
    print_minimizations(&text, kind);
}
