app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stderr

main! = || {
    zero = 0
    Stdout.line!("zero: ${zero}")
}
