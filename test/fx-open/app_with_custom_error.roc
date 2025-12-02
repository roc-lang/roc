app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test: both Exit and CustomError in different branches
# This triggers the type error
main! = |args| {
    if List.is_empty(args) {
        Err(Exit(42))
    } else {
        Err(CustomError)
    }
}
