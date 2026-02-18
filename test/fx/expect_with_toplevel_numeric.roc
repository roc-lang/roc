app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    Stdout.line!("hello")
}

x = 42
y = x
expect y == 42
