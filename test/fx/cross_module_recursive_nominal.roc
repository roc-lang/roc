app [main!] {
    pf: platform "./platform/main.roc",
    elem: "./elem_pkg/main.roc",
}

import pf.Stdout
import elem.Elem

main! = || {
    tree = Elem.div([])

    match tree {
        Div(_) => Stdout.line!("Div (correct)")
        Text(_) => Stdout.line!("Text (WRONG)")
    }
}
