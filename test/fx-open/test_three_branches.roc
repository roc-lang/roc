app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test: Three different error tags across branches
# Expected: Should work with open union
main! = |args| {
    match List.len(args) {
        0 => Err(Exit(42))
        1 => Err(CustomError)
        _ => Err(AnotherError)
    }
}
