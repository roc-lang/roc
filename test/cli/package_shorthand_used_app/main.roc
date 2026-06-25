app [main!] {
    pf: platform "../../fx/platform/main.roc",
    pkg: "../package_shorthand_used_pkg/main.roc",
}

import pf.Stdout
import pkg.Thing

main! = || {
    Stdout.line!(Thing.greeting)
}
