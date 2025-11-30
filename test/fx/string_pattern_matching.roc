app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Tests pattern matching on string literals in match expressions.

main! = || {
    greet("Alice")
    greet("Bob")
}

greet = |name| {
    message = match name {
        "Alice" => "Hello Alice!"
        "Bob" => "Hey Bob!"
        _ => "Hello stranger!"
    }
    Stdout.line!(message)
}
