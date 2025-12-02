app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test: Closed union - both branches return Exit (same tag)
# Expected: Should work fine
main! = |args| {
    if List.is_empty(args) {
        Err(Exit(42))
    } else {
        Err(Exit(1))
    }
}
