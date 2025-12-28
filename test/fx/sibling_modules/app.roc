app [main!] { pf: platform "../platform/main.roc" }

import pf.Stdout
import Helper

main! = || {
    Stdout.line!(Helper.greet("World"))
}
