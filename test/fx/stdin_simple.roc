app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin

main! = || Stdin.line!()
