app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stdin

main! = || {
    Stdout.line!("Before stdin")
    Stdin.line!()
    Stdout.line!("After stdin")
}
