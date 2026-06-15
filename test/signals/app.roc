app [main!] { pf: platform "./platform/main.roc" }

import pf.Elem
import pf.NodeValue exposing [NodeValue]
import pf.Reactive

Counter := { count : I64 }.{
    init : I64 -> Counter
    init = |count| { count: count }

    codec : Reactive.Codec(Counter)
    codec = Reactive.Codec.make(Counter.encode, Counter.decode)

    encode : Counter -> NodeValue
    encode = |counter| NodeValue.from_i64(counter.count)

    decode : NodeValue -> Try(Counter, [TypeMismatch])
    decode = |nv| {
        (result, _) = NodeValue.decode_i64(NodeValue.format, nv)
        match result {
            Ok(count) => Ok(Counter.init(count))
            Err(_) => Err(TypeMismatch)
        }
    }

    render! : Reactive.Signal(Counter) => Elem.Component(Counter)
    render! = |state| {
        { sender: dec_send, receiver: dec_clicks } = Reactive.Event.channel_unit!()
        { sender: inc_send, receiver: inc_clicks } = Reactive.Event.channel_unit!()

        dec_changes =
            Reactive.Event.with_latest_unit(
                dec_clicks,
                state,
                Counter.codec,
                Counter.codec,
                |counter| Counter.init(counter.count - 1),
            )

        inc_changes =
            Reactive.Event.with_latest_unit(
                inc_clicks,
                state,
                Counter.codec,
                Counter.codec,
                |counter| Counter.init(counter.count + 1),
            )

        changes = Reactive.Event.merge(dec_changes, inc_changes)

        count = Reactive.Signal.map(state, Counter.codec, Reactive.Codec.i64, |counter| counter.count)
        count_label = Reactive.Signal.map_i64_to_str(count, |n| n.to_str())

        Elem.component(
            Elem.div([
                Elem.button({ on_click: dec_send, label: Reactive.Signal.const_str("-") }),
                Elem.label(count_label),
                Elem.button({ on_click: inc_send, label: Reactive.Signal.const_str("+") }),
            ]),
            changes,
        )
    }
}

App := { left : Counter, right : Counter }.{
    init : {} -> App
    init = |_| App.make(Counter.init(0), Counter.init(0))

    make : Counter, Counter -> App
    make = |left, right| { left: left, right: right }

    codec : Reactive.Codec(App)
    codec = Reactive.Codec.make(App.encode, App.decode)

    encode : App -> NodeValue
    encode = |model| {
        NodeValue.from_list([
            NodeValue.from_i64(model.left.count),
            NodeValue.from_i64(model.right.count),
        ])
    }

    decode : NodeValue -> Try(App, [TypeMismatch])
    decode = |nv| {
        left_result =
            match NodeValue.list_get(nv, 0) {
                Ok(left_nv) => Counter.decode(left_nv)
                Err(_) => Err(TypeMismatch)
            }

        right_result =
            match NodeValue.list_get(nv, 1) {
                Ok(right_nv) => Counter.decode(right_nv)
                Err(_) => Err(TypeMismatch)
            }

        match (left_result, right_result) {
            (Ok(left), Ok(right)) => Ok(App.make(left, right))
            _ => Err(TypeMismatch)
        }
    }

    render! : Reactive.Signal(App) => Elem.Component(App)
    render! = |state| {
        left_render! =
            Elem.translate(
                App.codec,
                Counter.codec,
                Counter.render!,
                |model| model.left,
                |model, counter| App.make(counter, model.right),
            )

        right_render! =
            Elem.translate(
                App.codec,
                Counter.codec,
                Counter.render!,
                |model| model.right,
                |model, counter| App.make(model.left, counter),
            )

        left_component = left_render!(state)
        right_component = right_render!(state)

        changes =
            Reactive.Event.merge(
                Elem.Component.changes(left_component),
                Elem.Component.changes(right_component),
            )

        total = Reactive.Signal.map(state, App.codec, Reactive.Codec.i64, |model| model.left.count + model.right.count)
        total_label = Reactive.Signal.map_i64_to_str(total, |n| n.to_str())

        Elem.component(
            Elem.div([
                Elem.text("Left"),
                Elem.Component.elem(left_component),
                Elem.text("Right"),
                Elem.Component.elem(right_component),
                Elem.text("Total"),
                Elem.label(total_label),
            ]),
            changes,
        )
    }
}

main! : () => {}
main! = || {
    Elem.run_component!(App.codec, App.init({}), App.render!)
}
