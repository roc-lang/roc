app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Node := [
    Text(Str),
    Element(Str, List(Node)),
]

main! = || {
    text_node : Node
    text_node = Text("hello")
    children : List(Node)
    children = [text_node]
    match List.first(children) {
        Ok(child) =>
            match child {
                Text(_) => Stdout.line!("Text")
                Element(_, _) => Stdout.line!("Element")
            }
        Err(_) => Stdout.line!("Err")
    }
}
