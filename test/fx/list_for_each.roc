app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Tests List.for_each! with effectful callbacks

main! = || {
    items = ["apple", "banana", "cherry"]
    List.for_each!(items, |item| {
        Stdout.line!("Item: ${item}")
    })
}
