app [main!] {
    pf: platform "../../fx-open/platform/main.roc",
    pkg: "pkg/main.roc",
}

import pf.Stdout
import pkg.Helpers
import pkg.Callbacks
import pkg.Container

main! = |args| {
    value = args.len() - 1
    first = Container.({ callback: Callbacks.one })
    second = Container.({ callback: Callbacks.two })
    Stdout.line!(Helpers.apply_container(first, value))
    Stdout.line!(Helpers.apply_container(second, value))
    Stdout.line!(Helpers.apply_record({ inner: { callback: Callbacks.one } }, value))
    Stdout.line!(Helpers.apply_record({ inner: { callback: Callbacks.two } }, value))
    Ok({})
}
