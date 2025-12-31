app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! : List(Str) => Try({}, [Exit(I32)])
main! = |args| {
    match args {
        _ => {
            Stdout.line!("Invalid arguments")
            Err(Exit(1))
        }
    }
}
