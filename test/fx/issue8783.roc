app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test: recursive opaque type with recursive function through List.fold
# This is a regression test for issue #8783
Elem := [
    Element(Str, List(Elem)),
    Text(Str),
]

process_child : Str, Elem -> Str
process_child = |acc, child|
    "${acc} ${process(child)}"

process : Elem -> Str
process = |elem|
    match elem {
        Element(tag, children) =>
            "${tag}:${List.fold(children, "", process_child)}"
        Text(content) => content
    }

main! = || {
    elem : Elem
    elem = Element("div", [Text("hello")])
    result = process(elem)
    Stdout.line!("Result: ${result}")
}
