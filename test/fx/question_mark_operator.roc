app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Host

# Tests the `?` operator for error propagation.
# The operator unwraps Ok values or early-returns Err values.

get_greeting : Str -> Try(Str, [ListWasEmpty])
get_greeting = |greeting| {
    first = List.first([greeting])?
    Ok(first)
}

main! = || {
    runtime = Host.get_greeting!(Host.new("question"))
    selected = if Str.count_utf8_bytes(runtime) > 0 { "hello" } else { "missing" }

    match get_greeting(selected) {
        Ok(greeting) => Stdout.line!(greeting)
        Err(ListWasEmpty) => Stdout.line!("List was empty!")
    }
}
