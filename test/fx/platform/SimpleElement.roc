import Stdout

SimpleElement := [
    Container(List(SimpleElement)),
    Leaf({ value : Str }),
    Text(Str),
].{
    container : List(SimpleElement) -> SimpleElement
    container = |children| Container(children)

    leaf : Str -> SimpleElement
    leaf = |s| Leaf({ value: s })

    text : Str -> SimpleElement
    text = |s| Text(s)

    process! : SimpleElement => {}
    process! = |elem| {
        match elem {
            Container(children) => {
                Stdout.line!("Container branch")
                List.for_each!(children, |child| {
                    Stdout.line!("  iterating child")
                    SimpleElement.process!(child)
                })
            }
            Leaf(_payload) => {
                Stdout.line!("Leaf branch")
            }
            Text(s) => {
                Stdout.line!("Text branch: ${s}")
            }
        }
    }
}
