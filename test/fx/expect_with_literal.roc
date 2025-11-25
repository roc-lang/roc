app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stderr

main! = || {
    Stdout.line!("done")
}

answer : I64
answer = 42

# This tests that numeric literals in expect are properly typed
expect answer == 42
