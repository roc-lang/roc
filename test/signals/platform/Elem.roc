import Signal exposing [Signal]
import EventSender exposing [EventSender]
import SignalNode exposing [SignalNode]
import EventNode exposing [EventNode]
import Host

## UI Element tree containing signal and event graphs.
## Note: Label uses record wrapper to work around list iteration bug with opaque types
Elem := [
    Div(List(Elem)),
    Button({ on_click : EventNode, label : SignalNode }),
    Label({ signal : SignalNode }),
    Text(Str),
].{
    ## Walk the element tree, creating DOM elements and graph nodes
    ## parent_id is an ElemId (U64) from the host
    walk! : Elem, U64 => {}
    walk! = |elem, parent_id| {
        match elem {
            Div(children) => {
                div_id = Host.create_element!("div")
                Host.append_child!(parent_id, div_id)
                List.for_each!(children, |child| {
                    Elem.walk!(child, div_id)
                })
            }

            Button({ on_click, label: btn_label }) => {
                btn_id = Host.create_element!("button")
                Host.append_child!(parent_id, btn_id)

                label_node_id = SignalNode.walk!(btn_label)
                Host.bind_text!(btn_id, label_node_id)

                click_node_id = EventNode.walk!(on_click)
                Host.bind_click!(btn_id, click_node_id)
            }

            Label({ signal: text_signal }) => {
                span_id = Host.create_element!("span")
                Host.append_child!(parent_id, span_id)

                text_node_id = SignalNode.walk!(text_signal)
                Host.bind_text!(span_id, text_node_id)
            }

            Text(s) => {
                span_id = Host.create_element!("span")
                Host.set_text!(span_id, s)
                Host.append_child!(parent_id, span_id)
            }
        }
    }

    ## Create a div element containing children
    div : List(Elem) -> Elem
    div = |children| Div(children)

    ## Create a button element that fires an event when clicked
    button : { on_click : EventSender({}), label : Signal(Str) } -> Elem
    button = |config| {
        Button({
            on_click: EventSender.to_node(config.on_click),
            label: Signal.to_node(config.label),
        })
    }

    ## Create a label element displaying reactive text
    label : Signal(Str) -> Elem
    label = |text_signal| Label({ signal: Signal.to_node(text_signal) })

    ## Create a static text element
    text : Str -> Elem
    text = |s| Text(s)
}
