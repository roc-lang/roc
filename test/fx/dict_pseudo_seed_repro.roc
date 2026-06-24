app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    dict = Dict.single("a", "b")

    match Dict.get(dict, "a") {
        Ok(value) => Stdout.line!(value)
        Err(_) => Stdout.line!("missing")
    }
}
