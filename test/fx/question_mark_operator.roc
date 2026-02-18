app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Tests the `?` operator for error propagation.
# The operator unwraps Ok values or early-returns Err values.

get_greeting : {} -> Try(Str, [ListWasEmpty])
get_greeting = |{}| {
    first = List.first(["hello"])?
    Ok(first)
}

main! = || {
    match get_greeting({}) {
        Ok(greeting) => Stdout.line!(greeting)
        Err(ListWasEmpty) => Stdout.line!("List was empty!")
    }
}
