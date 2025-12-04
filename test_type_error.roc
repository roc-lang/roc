app [main!] { pf: platform "test/fx/platform/main.roc" }

import pf.Stdout

main! = || {
    x = 42
    Stdout.line!(x)
}
