app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    ["hello"].for_each!(Stdout.line!)
}
