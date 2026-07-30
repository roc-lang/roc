app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Host

Node := [
    Text(Str),
    Element(Str, List(Node)),
]

main! = || {
    runtime = Host.get_greeting!(Host.new("node"))
    content = if Str.count_utf8_bytes(runtime) > 0 { "hello" } else { "missing" }

    text_node : Node
    text_node = Text(content)
    children : List(Node)
    children = if Str.count_utf8_bytes(runtime) > 0 { [text_node] } else { [] }
    match List.first(children) {
        Ok(child) =>
            match child {
                Text(_) => Stdout.line!("Text")
                Element(_, _) => Stdout.line!("Element")
            }
        Err(_) => Stdout.line!("Err")
    }
}
