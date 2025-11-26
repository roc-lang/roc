app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    dbg "this should work now"
}
