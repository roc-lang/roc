import NodeValue exposing [NodeValue]
import EventNode exposing [EventNode]
import EventSender exposing [EventSender]
import Host

## An Event represents discrete occurrences over time.
## Uses record wrapper pattern for structural lifting to work.
Event(a) := { node : EventNode }.{
    ## Get the inner EventNode (for platform use)
    to_node : Event(a) -> EventNode
    to_node = |event| event.node

    ## Create a channel - returns sender/receiver pair
    ## Effectful: eagerly creates host event source so sender and receiver share the same node
    channel! : {} => { sender : EventSender(a), receiver : Event(a) }
    channel! = |{}| {
        host_id = Host.create_event_source!({})
        event_node = EventNode.make_prebuilt(host_id)
        {
            sender: { node: event_node },
            receiver: { node: event_node },
        }
    }

    ## Transform event payloads using a function (polymorphic)
    map : Event(a), (a -> b) -> Event(b)
        where [
            a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
            b.encode : b, NodeValue -> Try(NodeValue, []),
        ]
    map = |event, f| {
        A : a
        B : b
        source = event.node
        wrapped : NodeValue -> NodeValue
        wrapped = |input_nv| {
            (decode_result, _) = A.decode(NodeValue.format, input_nv)
            typed_input =
                match decode_result {
                    Ok(val) => val
                    Err(_) => ...
                }
            typed_output = f(typed_input)
            encode_result = B.encode(typed_output, NodeValue.format)
            match encode_result {
                Ok(output_nv) => output_nv
            }
        }

        { node: EventNode.make_map_event(source, Box.box(wrapped)) }
    }

    ## Map unit event to I64
    map_unit_to_i64 : Event({}), ({} -> I64) -> Event(I64)
    map_unit_to_i64 = |event, f| {
        source = event.node
        wrapped : NodeValue -> NodeValue
        wrapped = |_nv| NodeValue.from_i64(f({}))

        { node: EventNode.make_map_event(source, Box.box(wrapped)) }
    }

    ## Merge two event streams - fires when either fires
    merge : Event(a), Event(a) -> Event(a)
    merge = |left, right| {
        { node: EventNode.make_merge(left.node, right.node) }
    }
}
