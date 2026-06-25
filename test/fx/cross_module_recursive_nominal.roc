app [main!] {
    pf: platform "./platform/main.roc",
    elem: "./elem_pkg/main.roc",
}

import pf.Stdout
import elem.Elem
import pf.Host

main! = || {
    runtime = Host.get_greeting!(Host.new("tree"))
    children = if Str.count_utf8_bytes(runtime) > 0 { [] } else { [Elem.text("missing")] }
    tree = Elem.div(children)

    match tree {
        Div(_) => Stdout.line!("Div (correct)")
        Text(_) => Stdout.line!("Text (WRONG)")
    }
}
