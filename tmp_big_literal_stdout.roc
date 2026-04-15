app [main!] { pf: platform "./test/fx/platform/main.roc" }

import pf.Stdout

main! = || {
    Stdout.line!("My favourite color is Red")
}
