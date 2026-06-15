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

Item := { id : Str, label : Str }.{
    make : Str, Str -> Item
    make = |id, label| { id, label }

    codec : Reactive.Codec(Item)
    codec = Reactive.Codec.make(Item.encode, Item.decode)

    encode : Item -> NodeValue
    encode = |item| {
        NodeValue.from_list([
            NodeValue.from_str(item.id),
            NodeValue.from_str(item.label),
        ])
    }

    decode : NodeValue -> Try(Item, [TypeMismatch])
    decode = |nv| {
        id_result =
            match NodeValue.list_get(nv, 0) {
                Ok(id_nv) => {
                    (result, _) = NodeValue.decode_str(NodeValue.format, id_nv)
                    result
                }
                Err(_) => Err(TypeMismatch)
            }

        label_result =
            match NodeValue.list_get(nv, 1) {
                Ok(label_nv) => {
                    (result, _) = NodeValue.decode_str(NodeValue.format, label_nv)
                    result
                }
                Err(_) => Err(TypeMismatch)
            }

        match (id_result, label_result) {
            (Ok(id), Ok(label)) => Ok(Item.make(id, label))
            _ => Err(TypeMismatch)
        }
    }
}

App := { left : Counter, right : Counter }.{
    init : {} -> App
    init = |_| App.make(Counter.init(0), Counter.init(0))

    make : Counter, Counter -> App
    make = |left, right| { left, right }

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

    local_counter : Str, I64 => { elem : Elem, count : Reactive.Signal(I64) }
    local_counter = |name, initial| {
        { sender: dec_send, receiver: dec_clicks } = Reactive.Event.channel_unit!()
        { sender: inc_send, receiver: inc_clicks } = Reactive.Event.channel_unit!()

        decs = Reactive.Event.map_unit_to_i64(dec_clicks, |_| -1)
        incs = Reactive.Event.map_unit_to_i64(inc_clicks, |_| 1)
        changes = Reactive.Event.merge(decs, incs)
        count = Reactive.Signal.fold_i64(initial, changes, |current, delta| current + delta)
        count_label = Reactive.Signal.map_i64_to_str(count, |n| n.to_str())

        {
            elem: Elem.div([
                Elem.text(name),
                Elem.button({ on_click: dec_send, label: Reactive.Signal.const_str("-") }),
                Elem.label(count_label),
                Elem.button({ on_click: inc_send, label: Reactive.Signal.const_str("+") }),
            ]),
            count,
        }
    }

    render_item :
        Reactive.EventSender({}),
        Reactive.EventSender({}),
        Item
        => Elem
    render_item = |mount_send, unmount_send, item| {
        { sender: inc_send, receiver: inc_clicks } = Reactive.Event.channel_unit!()
        incs = Reactive.Event.map_unit_to_i64(inc_clicks, |_| 1)
        count = Reactive.Signal.fold_i64(0, incs, |current, delta| current + delta)
        count_label = Reactive.Signal.map_i64_to_str(count, |n| n.to_str())

        Elem.div([
            Elem.lifecycle({ on_mount: mount_send, on_unmount: unmount_send }),
            Elem.text(item.label),
            Elem.button({ on_click: inc_send, label: Reactive.Signal.const_str("+") }),
            Elem.label(count_label),
        ])
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

        controlled_left = left_render!(state)
        controlled_right = right_render!(state)
        controlled_changes =
            Reactive.Event.merge(
                Elem.Component.changes(controlled_left),
                Elem.Component.changes(controlled_right),
            )
        controlled_total = Reactive.Signal.map(state, App.codec, Reactive.Codec.i64, |model| model.left.count + model.right.count)
        controlled_total_label = Reactive.Signal.map_i64_to_str(controlled_total, |n| n.to_str())

        local_left = App.local_counter("Local left", 0)
        local_right = App.local_counter("Local right", 10)
        local_total =
            Reactive.Signal.map2(
                local_left.count,
                Reactive.Codec.i64,
                local_right.count,
                Reactive.Codec.i64,
                Reactive.Codec.i64,
                |left, right| left + right,
            )
        local_total_label = Reactive.Signal.map_i64_to_str(local_total, |n| n.to_str())
        diamond_left = Reactive.Signal.map(local_total, Reactive.Codec.i64, Reactive.Codec.i64, |n| n + 1)
        diamond_right = Reactive.Signal.map(local_total, Reactive.Codec.i64, Reactive.Codec.i64, |n| n * 10)
        diamond_label =
            Reactive.Signal.map2(
                diamond_left,
                Reactive.Codec.i64,
                diamond_right,
                Reactive.Codec.i64,
                Reactive.Codec.str,
                |left, right| Str.concat(Str.concat(left.to_str(), "/"), right.to_str()),
            )

        { sender: merge_send, receiver: merge_clicks } = Reactive.Event.channel_unit!()
        merge_left = Reactive.Event.map_unit_to_i64(merge_clicks, |_| 1)
        merge_right = Reactive.Event.map_unit_to_i64(merge_clicks, |_| 10)
        merge_count = Reactive.Signal.fold_i64(0, Reactive.Event.merge(merge_left, merge_right), |current, delta| current + delta)
        merge_label = Reactive.Signal.map_i64_to_str(merge_count, |n| n.to_str())

        { sender: toggle_send, receiver: toggle_clicks } = Reactive.Event.channel_unit!()
        visible = Reactive.Signal.fold(Reactive.Codec.bool, True, toggle_clicks, Reactive.Codec.unit, |current, _| !current)
        { sender: dyn_mount_send, receiver: dyn_mounts } = Reactive.Event.channel_unit!()
        { sender: dyn_unmount_send, receiver: dyn_unmounts } = Reactive.Event.channel_unit!()
        dyn_mount_count = Reactive.Signal.fold_i64(0, Reactive.Event.map_unit_to_i64(dyn_mounts, |_| 1), |current, delta| current + delta)
        dyn_unmount_count = Reactive.Signal.fold_i64(0, Reactive.Event.map_unit_to_i64(dyn_unmounts, |_| 1), |current, delta| current + delta)

        { sender: reorder_send, receiver: reorder_clicks } = Reactive.Event.channel_unit!()
        { sender: remove_send, receiver: remove_clicks } = Reactive.Event.channel_unit!()
        reordered_items =
            Reactive.Event.map(
                reorder_clicks,
                Reactive.Codec.unit,
                Reactive.Codec.list(Item.codec),
                |_| [Item.make("b", "Beta"), Item.make("a", "Alpha")],
            )
        removed_items =
            Reactive.Event.map(
                remove_clicks,
                Reactive.Codec.unit,
                Reactive.Codec.list(Item.codec),
                |_| [Item.make("a", "Alpha")],
            )
        item_commands = Reactive.Event.merge(reordered_items, removed_items)
        items =
            Reactive.Signal.fold(
                Reactive.Codec.list(Item.codec),
                [Item.make("a", "Alpha"), Item.make("b", "Beta")],
                item_commands,
                Reactive.Codec.list(Item.codec),
                |_current, next| next,
            )
        { sender: item_mount_send, receiver: item_mounts } = Reactive.Event.channel_unit!()
        { sender: item_unmount_send, receiver: item_unmounts } = Reactive.Event.channel_unit!()
        item_mount_count = Reactive.Signal.fold_i64(0, Reactive.Event.map_unit_to_i64(item_mounts, |_| 1), |current, delta| current + delta)
        item_unmount_count = Reactive.Signal.fold_i64(0, Reactive.Event.map_unit_to_i64(item_unmounts, |_| 1), |current, delta| current + delta)

        Elem.component(
            Elem.div([
                Elem.text("Controlled"),
                Elem.Component.elem(controlled_left),
                Elem.Component.elem(controlled_right),
                Elem.text("Controlled total"),
                Elem.label(controlled_total_label),

                Elem.text("Local"),
                local_left.elem,
                local_right.elem,
                Elem.text("Local total"),
                Elem.label(local_total_label),
                Elem.text("Diamond"),
                Elem.label(diamond_label),
                Elem.text("Merge"),
                Elem.button({ on_click: merge_send, label: Reactive.Signal.const_str("merge") }),
                Elem.label(merge_label),

                Elem.text("Dynamic"),
                Elem.label(Reactive.Signal.map_i64_to_str(dyn_mount_count, |n| n.to_str())),
                Elem.label(Reactive.Signal.map_i64_to_str(dyn_unmount_count, |n| n.to_str())),
                Elem.button({ on_click: toggle_send, label: Reactive.Signal.const_str("toggle") }),
                Elem.when(
                    visible,
                    Elem.div([
                        Elem.lifecycle({ on_mount: dyn_mount_send, on_unmount: dyn_unmount_send }),
                        Elem.text("Dynamic on"),
                    ]),
                    Elem.div([Elem.text("Dynamic off")]),
                ),

                Elem.text("Keyed"),
                Elem.label(Reactive.Signal.map_i64_to_str(item_mount_count, |n| n.to_str())),
                Elem.label(Reactive.Signal.map_i64_to_str(item_unmount_count, |n| n.to_str())),
                Elem.button({ on_click: reorder_send, label: Reactive.Signal.const_str("reorder") }),
                Elem.button({ on_click: remove_send, label: Reactive.Signal.const_str("remove") }),
                Elem.each(items, Item.codec, |item| item.id, |item| App.render_item(item_mount_send, item_unmount_send, item)),
            ]),
            controlled_changes,
        )
    }
}

main! : () => {}
main! = || {
    Elem.run_component!(App.codec, App.init({}), App.render!)
}
