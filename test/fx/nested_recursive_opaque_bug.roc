app [main!] {
    pf: platform "./platform/main.roc",
    pkg: "./nested_recursive_opaque_pkg/main.roc",
}

import pf.Stdout
import pkg.Outer

main! = || {
    tree = Outer.div([Outer.text("hello")])

    match tree {
        Div(_) => Stdout.line!("Div (correct)")
        _ => Stdout.line!("other")
    }
}
