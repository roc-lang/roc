app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

main! = || {
    Stdout.line!("Before stdin")
    _line = Stdin.line!()
    Stdout.line!("After stdin")
}
