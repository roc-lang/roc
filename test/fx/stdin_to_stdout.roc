app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

main! = || Stdout.line!(Stdin.line!())
