app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stderr

main! : () => {}
main! = ||
    Stdout.line!("Hello from stdout!")
    Stderr.line!("Error from stderr!")
    Stdout.line!("Line 1 to stdout")
    Stderr.line!("Line 2 to stderr")
    Stdout.line!("Line 3 to stdout")
    {}
