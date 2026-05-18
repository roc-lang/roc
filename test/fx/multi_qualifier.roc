app [main!] {
    fx: platform "./platform/main.roc",
    hlp: "./helper_pkg/main.roc",
}

import fx.Stdout
import hlp.Helper

main! = || {
    greeting = Helper.greet("World")
    Stdout.line!(greeting)
}
