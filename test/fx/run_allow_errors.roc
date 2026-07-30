app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    Stdout.line!("Hello, World!")

    # This checked error must become a crash only when execution reaches it.
    x = y
}
