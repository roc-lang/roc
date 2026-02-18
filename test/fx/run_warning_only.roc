app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    # This has only a warning - unused variable
    x = 5
    Stdout.line!("Hello, World!")
}
