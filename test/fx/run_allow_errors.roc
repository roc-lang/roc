app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    Stdout.line!("Hello, World!")

    # Intentionally provoke errors to test --allow-errors
    x = y
}
