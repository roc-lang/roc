import Reactive
import Graph
import Host

## UI Element tree containing signal and event graphs.
Elem := [
    Div(List(Elem)),
    Button({ on_click : Graph.EventNode, label : Graph.SignalNode }),
    Label({ signal : Graph.SignalNode }),
    Text(Str),
].{
    Component(a) := { elem : Elem, changes : Reactive.Event(a) }.{
        elem : Component(a) -> Elem
        elem = |component| component.elem

        changes : Component(a) -> Reactive.Event(a)
        changes = |component| component.changes
    }

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

                label_node_id = Graph.SignalNode.walk!(btn_label)
                Host.bind_text!(btn_id, label_node_id)

                click_node_id = Graph.EventNode.walk!(on_click)
                Host.bind_click!(btn_id, click_node_id)
            }

            Label({ signal: text_signal }) => {
                span_id = Host.create_element!("span")
                Host.append_child!(parent_id, span_id)

                text_node_id = Graph.SignalNode.walk!(text_signal)
                Host.bind_text!(span_id, text_node_id)
            }

            Text(s) => {
                span_id = Host.create_element!("span")
                Host.set_text!(span_id, s)
                Host.append_child!(parent_id, span_id)
            }
        }
    }

    run! : Elem => {}
    run! = |elem| {
        root = Host.create_root!()
        Elem.walk!(elem, root)
    }

    run_component! : Reactive.Codec(a), a, (Reactive.Signal(a) => Component(a)) => {}
    run_component! = |codec, initial, render!| {
        state = Reactive.Signal.state!(codec, initial)
        component = render!(state)

        root = Host.create_root!()
        Elem.walk!(Component.elem(component), root)

        state_id = Graph.SignalNode.walk!(Reactive.Signal.to_node(state))
        changes_id = Graph.EventNode.walk!(Reactive.Event.to_node(Component.changes(component)))
        Host.bind_signal_update!(state_id, changes_id)
    }

    component : Elem, Reactive.Event(a) -> Component(a)
    component = |elem, changes| { elem: elem, changes: changes }

    translate :
        Reactive.Codec(parent),
        Reactive.Codec(child),
        (Reactive.Signal(child) => Component(child)),
        (parent -> child),
        (parent, child -> parent)
        -> (Reactive.Signal(parent) => Component(parent))
    translate = |parent_codec, child_codec, child_render!, getter, setter| {
        |parent_signal| {
            child_signal = Reactive.Signal.map(parent_signal, parent_codec, child_codec, getter)
            child_component = child_render!(child_signal)
            parent_changes =
                Reactive.Event.with_latest(
                    Component.changes(child_component),
                    child_codec,
                    parent_signal,
                    parent_codec,
                    parent_codec,
                    |child, parent| setter(parent, child),
                )

            Elem.component(Component.elem(child_component), parent_changes)
        }
    }

    div : List(Elem) -> Elem
    div = |children| Div(children)

    button : { on_click : Reactive.EventSender({}), label : Reactive.Signal(Str) } -> Elem
    button = |config| {
        Button({
            on_click: Reactive.EventSender.to_node(config.on_click),
            label: Reactive.Signal.to_node(config.label),
        })
    }

    label : Reactive.Signal(Str) -> Elem
    label = |text_signal| Label({ signal: Reactive.Signal.to_node(text_signal) })

    text : Str -> Elem
    text = |s| Text(s)
}
