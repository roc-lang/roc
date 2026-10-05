app [main!] {
    pf: platform "../../fx-open/platform/main.roc",
    pkg: "pkg/main.roc",
}

import pf.Stdout
import pkg.Helpers
import pkg.Callbacks

main! = |args| {
    value = args.len() - 1
    first = Callbacks.make(value)
    second = Callbacks.make(value + 1)
    Stdout.line!(Helpers.apply(first, value))
    Stdout.line!(Helpers.apply(second, value))
    Stdout.line!(Helpers.apply(first, value))
    Stdout.line!(Helpers.apply(second, value))
    Ok({})
}
