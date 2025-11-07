app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stderr

main! = |_|
    Stdout.line!("Hello from stdout!")
    Stdout.line!("Line 1 to stdout")
    Stderr.line!("Line 2 to stderr")
    Stdout.line!("Line 3 to stdout")
    Stderr.line!("Error from stderr!")
