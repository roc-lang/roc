app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# The children list comes from pattern matching on Element
Elem := [
    Element(Str, List(Elem)),
    Text(Str),
]

main! = || {
    elem : Elem
    elem = Element("div", [Text("hello")])

    children = match elem {
        Element(_tag, c) => c
        Text(_) => []
    }

    var $count = 0
    for child in children {
        $count = match child {
            Text(_) => $count + 1
            Element(_, _) => $count + 10
        }
    }
    Stdout.line!("Count: ${I64.to_str($count)}")
}
