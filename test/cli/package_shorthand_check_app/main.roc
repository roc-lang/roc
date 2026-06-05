app [main!] {
    pf: platform "../../fx/platform/main.roc",
    pkg: "../package_shorthand_check_pkg/main.roc",
}

import pf.Stdout

main! = || {
    Stdout.line!("hello")
}
