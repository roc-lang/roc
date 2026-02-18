app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    expect 1 == 1
    Stdout.line!("All good.")
}
