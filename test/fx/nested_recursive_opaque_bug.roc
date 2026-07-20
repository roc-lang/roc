app [main!] {
    pf: platform "./platform/main.roc",
    pkg: "./nested_recursive_opaque_pkg/main.roc",
}

import pf.Stdout
import pf.Stdin
import pkg.Outer

main! = || {
    tree = Outer.div([Outer.text(Stdin.line!())])

    match tree {
        Div(_) => Stdout.line!("Div (correct)")
        _ => Stdout.line!("other")
    }
}
