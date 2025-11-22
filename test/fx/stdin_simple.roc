app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stderr

main! = || {
    line = Stdin.line!()
    Stderr.line!(line)
}
