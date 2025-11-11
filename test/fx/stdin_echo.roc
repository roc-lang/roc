app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

main! = || {
    line = Stdin.line!()
    Stdout.line!(line)
}
