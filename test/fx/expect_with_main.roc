app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stderr

main! = || {
    Stdout.line!("done")
}

answer : I64
answer = 42

fortytwo : I64
fortytwo = 42

expect answer == fortytwo
