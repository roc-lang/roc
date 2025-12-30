app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

## BUG: `dbg` corrupts recursive tag union variant discriminant
##
## When `dbg` is used on a recursive tag union value, the variant
## discriminant gets corrupted. A `Text` variant becomes `Element`
## when later pattern matched.
##
## Without dbg: Child is Text: hello   (correct)
## With dbg:    Child is Element: hello (BUG!)

Node := [
    Text(Str),
    Element(Str, List(Node)),
].{
    text : Str -> Node
    text = |content| {
        result = Text(content)
        dbg result
        result
    }

    element : Str, List(Node) -> Node
    element = |tag, children| {
        result = Element(tag, children)
        dbg result
        result
    }
}

main! = || {
    text_node = Node.text("hello")
    elem = Node.element("div", [text_node])

    match elem {
        Element(_tag, children) =>
            match List.first(children) {
                Ok(child) =>
                    match child {
                        Text(content) => Stdout.line!("Child is Text: ${content}")
                        Element(tag, _) => Stdout.line!("Child is Element: ${tag}")
                    }
                Err(_) => Stdout.line!("No children")
            }
        Text(_) => Stdout.line!("Root is Text (unexpected)")
    }
}
