app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Bug 12: String literal matching in match doesn't work
# Expected: "Hello Alice!" then "Hey Bob!"
# Actual: "Hello stranger!" for both (always falls through to wildcard)

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
