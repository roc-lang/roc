app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Minimal test: match on first element of list from pattern matching
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

    result = match children {
        [child, ..] => match child {
            Text(_) => "text"
            Element(_, _) => "element"
        }
        [] => "empty"
    }

    Stdout.line!("Result: ${result}")
}
