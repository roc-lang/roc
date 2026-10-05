app [main!] {
    pf: platform "../../fx-open/platform/main.roc",
    pkg: "pkg/main.roc",
}

import pf.Stdout
import pkg.Helpers
import pkg.Callbacks

main! = |args| {
    value = args.len() - 1
    Stdout.line!(Helpers.apply(Callbacks.one, value))
    Stdout.line!(Helpers.apply(Callbacks.two, value))
    Stdout.line!(Helpers.apply(Callbacks.one, value))
    Stdout.line!(Helpers.apply(Callbacks.two, value))
    Ok({})
}
