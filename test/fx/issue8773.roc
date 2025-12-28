app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Regression test for issue #8773: TypeMismatch when pattern matching
# on the result of List.first with a recursive opaque type.

Item := [
    Element(Str, List(Item)),
    Text(Str),
]

main! = || {
    items : List(Item)
    items = [Text("hello"), Text("world")]

    match List.first(items) {
        Ok(item) =>
            match item {
                Element(tag, _) => Stdout.line!("Element: ${tag}")
                Text(content) => Stdout.line!("Text: ${content}")
            }
        Err(_) => Stdout.line!("Empty list")
    }
}
