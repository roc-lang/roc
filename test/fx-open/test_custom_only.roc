app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test: Only custom error tags (not Exit)
# Expected: Should work with open union
main! = |args| {
    if List.is_empty(args) {
        Err(CustomError)
    } else {
        Err(AnotherError)
    }
}
