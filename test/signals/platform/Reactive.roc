import NodeValue exposing [NodeValue]
import Graph
import Host

## Public reactive types. Signals and events carry typed graph nodes. Codecs are
## passed explicitly to graph combinators that need to cross the type-erased
## NodeValue boundary.
Reactive := [].{
    Codec(a) := { encode_fn : a -> NodeValue, decode_fn : NodeValue -> Try(a, [TypeMismatch]) }.{
        make : (a -> NodeValue), (NodeValue -> Try(a, [TypeMismatch])) -> Codec(a)
        make = |encode_fn, decode_fn| { encode_fn: encode_fn, decode_fn: decode_fn }

        encode : Codec(a), a -> NodeValue
        encode = |codec_value, value| {
            encode_fn = codec_value.encode_fn
            encode_fn(value)
        }

        decode : Codec(a), NodeValue -> Try(a, [TypeMismatch])
        decode = |codec_value, value| {
            decode_fn = codec_value.decode_fn
            decode_fn(value)
        }

        unit : Codec({})
        unit = Codec.make(|_| NodeValue.unit, |nv| {
            (result, _) = NodeValue.decode_unit(NodeValue.format, nv)
            result
        })

        i64 : Codec(I64)
        i64 = Codec.make(NodeValue.from_i64, |nv| {
            (result, _) = NodeValue.decode_i64(NodeValue.format, nv)
            result
        })

        str : Codec(Str)
        str = Codec.make(NodeValue.from_str, |nv| {
            (result, _) = NodeValue.decode_str(NodeValue.format, nv)
            result
        })
    }

    EventSender(a) := { node : Graph.EventNode }.{
        to_node : EventSender(a) -> Graph.EventNode
        to_node = |sender| sender.node
    }

    Event(a) := { node : Graph.EventNode }.{
        to_node : Event(a) -> Graph.EventNode
        to_node = |event| event.node

        from_node : Graph.EventNode -> Event(a)
        from_node = |node| { node: node }

        channel! : Codec(a) => { sender : EventSender(a), receiver : Event(a) }
        channel! = |_event_codec| {
            host_id = Host.create_event_source!()
            event_node = Graph.EventNode.make_prebuilt(host_id)
            {
                sender: { node: event_node },
                receiver: { node: event_node },
            }
        }

        channel_unit! : () => { sender : EventSender({}), receiver : Event({}) }
        channel_unit! = || Event.channel!(Codec.unit)

        map : Event(a), Codec(a), Codec(b), (a -> b) -> Event(b)
        map = |event, input_codec, output_codec, f| {
            source = event.node
            decode_input = input_codec.decode_fn
            encode_output = output_codec.encode_fn
            wrapped : NodeValue -> NodeValue
            wrapped = |input_nv| {
                typed_input =
                    match decode_input(input_nv) {
                        Ok(val) => val
                        Err(_) => ...
                    }
                typed_output = f(typed_input)
                encode_output(typed_output)
            }

            { node: Graph.EventNode.make_map_event(source, Box.box(wrapped)) }
        }

        map_unit_to_i64 : Event({}), ({} -> I64) -> Event(I64)
        map_unit_to_i64 = |event, f| {
            source = event.node
            wrapped : NodeValue -> NodeValue
            wrapped = |_nv| NodeValue.from_i64(f({}))

            { node: Graph.EventNode.make_map_event(source, Box.box(wrapped)) }
        }

        merge : Event(a), Event(a) -> Event(a)
        merge = |left, right| {
            { node: Graph.EventNode.make_merge(left.node, right.node) }
        }

        with_latest : Event(e), Codec(e), Signal(s), Codec(s), Codec(out), (e, s -> out) -> Event(out)
        with_latest = |event, event_codec, signal, signal_codec, output_codec, combine| {
            decode_event = event_codec.decode_fn
            decode_signal = signal_codec.decode_fn
            encode_output = output_codec.encode_fn
            wrapped : (NodeValue, NodeValue) -> NodeValue
            wrapped = |(event_nv, signal_nv)| {
                typed_event =
                    match decode_event(event_nv) {
                        Ok(val) => val
                        Err(_) => ...
                    }
                typed_signal =
                    match decode_signal(signal_nv) {
                        Ok(val) => val
                        Err(_) => ...
                    }
                output = combine(typed_event, typed_signal)
                encode_output(output)
            }

            {
                node: Graph.EventNode.make_with_latest(
                    event.node,
                    signal.node,
                    Box.box(wrapped),
                ),
            }
        }

        with_latest_unit : Event({}), Signal(s), Codec(s), Codec(out), (s -> out) -> Event(out)
        with_latest_unit = |event, signal, signal_codec, output_codec, combine| {
            decode_signal = signal_codec.decode_fn
            encode_output = output_codec.encode_fn
            wrapped : (NodeValue, NodeValue) -> NodeValue
            wrapped = |(_event_nv, signal_nv)| {
                typed_signal =
                    match decode_signal(signal_nv) {
                        Ok(val) => val
                        Err(_) => ...
                    }
                output = combine(typed_signal)
                encode_output(output)
            }

            {
                node: Graph.EventNode.make_with_latest(
                    event.node,
                    signal.node,
                    Box.box(wrapped),
                ),
            }
        }
    }

    Signal(a) := { node : Graph.SignalNode }.{
        to_node : Signal(a) -> Graph.SignalNode
        to_node = |signal| signal.node

        from_node : Graph.SignalNode -> Signal(a)
        from_node = |node| { node: node }

        state! : Codec(a), a => Signal(a)
        state! = |signal_codec, value| {
            nv = Codec.encode(signal_codec, value)
            host_id = Host.create_signal_state!(nv)
            { node: Graph.SignalNode.make_prebuilt_signal(host_id) }
        }

        const : Codec(a), a -> Signal(a)
        const = |signal_codec, value| {
            nv = Codec.encode(signal_codec, value)
            { node: Graph.SignalNode.make_const(nv) }
        }

        const_i64 : I64 -> Signal(I64)
        const_i64 = |value| {
            node: Graph.SignalNode.make_const(NodeValue.from_i64(value)),
        }

        const_str : Str -> Signal(Str)
        const_str = |value| {
            node: Graph.SignalNode.make_const(NodeValue.from_str(value)),
        }

        map : Signal(a), Codec(a), Codec(b), (a -> b) -> Signal(b)
        map = |signal, input_codec, output_codec, f| {
            source = signal.node
            decode_input = input_codec.decode_fn
            encode_output = output_codec.encode_fn
            wrapped : NodeValue -> NodeValue
            wrapped = |input_nv| {
                typed_input =
                    match decode_input(input_nv) {
                        Ok(val) => val
                        Err(_) => ...
                    }
                typed_output = f(typed_input)
                encode_output(typed_output)
            }

            { node: Graph.SignalNode.make_map_signal(source, Box.box(wrapped)) }
        }

        map_i64_to_str : Signal(I64), (I64 -> Str) -> Signal(Str)
        map_i64_to_str = |signal, f| {
            source = signal.node
            wrapped : NodeValue -> NodeValue
            wrapped = |nv| NodeValue.from_str(f(NodeValue.to_i64(nv)))

            { node: Graph.SignalNode.make_map_signal(source, Box.box(wrapped)) }
        }

        fold : Codec(a), a, Event(e), Codec(e), (a, e -> a) -> Signal(a)
        fold = |acc_codec, initial, event, event_codec, step_fn| {
            initial_nv = Codec.encode(acc_codec, initial)
            event_node = Event.to_node(event)
            decode_acc = acc_codec.decode_fn
            encode_acc = acc_codec.encode_fn
            decode_event = event_codec.decode_fn
            wrapped : (NodeValue, NodeValue) -> NodeValue
            wrapped = |(acc_nv, evt_nv)| {
                acc =
                    match decode_acc(acc_nv) {
                        Ok(val) => val
                        Err(_) => ...
                    }
                evt =
                    match decode_event(evt_nv) {
                        Ok(val) => val
                        Err(_) => ...
                    }
                new_acc = step_fn(acc, evt)
                encode_acc(new_acc)
            }

            {
                node: Graph.SignalNode.make_fold(
                    initial_nv,
                    event_node,
                    Box.box(wrapped),
                ),
            }
        }

        fold_i64 : I64, Event(I64), (I64, I64 -> I64) -> Signal(I64)
        fold_i64 = |initial, event, step_fn| {
            event_node = Event.to_node(event)
            wrapped : (NodeValue, NodeValue) -> NodeValue
            wrapped = |(acc_nv, evt_nv)| {
                acc = NodeValue.to_i64(acc_nv)
                evt = NodeValue.to_i64(evt_nv)
                NodeValue.from_i64(step_fn(acc, evt))
            }

            {
                node: Graph.SignalNode.make_fold(
                    NodeValue.from_i64(initial),
                    event_node,
                    Box.box(wrapped),
                ),
            }
        }

        zip_with : Signal(a), Codec(a), Event(e), Codec(e), (a, e -> a) -> Signal(a)
        zip_with = |signal, signal_codec, event, event_codec, combine| {
            decode_signal = signal_codec.decode_fn
            encode_signal = signal_codec.encode_fn
            decode_event = event_codec.decode_fn
            wrapped : (NodeValue, NodeValue) -> NodeValue
            wrapped = |(signal_nv, event_nv)| {
                typed_signal =
                    match decode_signal(signal_nv) {
                        Ok(val) => val
                        Err(_) => ...
                    }
                typed_event =
                    match decode_event(event_nv) {
                        Ok(val) => val
                        Err(_) => ...
                    }
                output = combine(typed_signal, typed_event)
                encode_signal(output)
            }

            {
                node: Graph.SignalNode.make_zip_with(
                    signal.node,
                    Event.to_node(event),
                    Box.box(wrapped),
                ),
            }
        }
    }
}
