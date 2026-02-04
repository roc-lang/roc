import NodeB exposing [NodeB]
import Stdout

Element := [
    Div(List(Element)),
    Label(NodeB),
    Text(Str),
].{
    div : List(Element) -> Element
    div = |children| Div(children)

    label : NodeB -> Element
    label = |node| Label(node)

    text : Str -> Element
    text = |s| Text(s)

    process! : Element => {}
    process! = |elem| {
        match elem {
            Div(children) => {
                Stdout.line!("Div branch")
                List.for_each!(children, |child| {
                    Stdout.line!("  iterating child")
                    Element.process!(child)
                })
            }
            Label(_node) => {
                Stdout.line!("Label branch")
            }
            Text(s) => {
                Stdout.line!("Text branch: ${s}")
            }
        }
    }
}
