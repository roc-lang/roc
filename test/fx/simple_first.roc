app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Simpler test: just List.first on a list of I64

main! = || {
    items : List(I64)
    items = [1, 2, 3]

    match List.first(items) {
        Ok(n) => Stdout.line!("Got: ${I64.to_str(n)}")
        Err(_) => Stdout.line!("Empty list")
    }
}
