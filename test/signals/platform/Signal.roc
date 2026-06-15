import NodeValue exposing [NodeValue]
import SignalNode exposing [SignalNode]
import Event exposing [Event]

## A Signal represents a time-varying value.
## The type parameter `a` is a phantom type for compile-time type safety.
## Uses record wrapper pattern for structural lifting to work.
Signal(a) := { node : SignalNode }.{
    ## Get the inner SignalNode (for platform use)
    to_node : Signal(a) -> SignalNode
    to_node = |signal| signal.node

    ## Create a constant signal (polymorphic version with where clause)
    ## Uses Roc's built-in encode pattern: value.encode(format) calls format.encode_TYPE(format, value)
    const : a -> Signal(a)
        where [a.encode : a, NodeValue -> Try(NodeValue, [])]
    const = |value| {
        A : a
        result = A.encode(value, NodeValue.format)
        nv =
            match result {
                Ok(encoded) => encoded
            }
        { node: SignalNode.make_const(nv) }
    }

    ## Create a constant signal from an I64
    const_i64 : I64 -> Signal(I64)
    const_i64 = |value| {
        node: SignalNode.make_const(NodeValue.from_i64(value)),
    }

    ## Create a constant signal from a Str
    const_str : Str -> Signal(Str)
    const_str = |value| {
        node: SignalNode.make_const(NodeValue.from_str(value)),
    }

    ## Transform a signal's values using a function (polymorphic)
    map : Signal(a), (a -> b) -> Signal(b)
        where [
            a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
            b.encode : b, NodeValue -> Try(NodeValue, []),
        ]
    map = |signal, f| {
        A : a
        B : b
        source = signal.node
        wrapped : NodeValue -> NodeValue
        wrapped = |input_nv| {
            (decode_result, _remainder) = A.decode(NodeValue.format, input_nv)
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

        { node: SignalNode.make_map_signal(source, Box.box(wrapped)) }
    }

    ## Transform a signal's I64 values to Str
    map_i64_to_str : Signal(I64), (I64 -> Str) -> Signal(Str)
    map_i64_to_str = |signal, f| {
        source = signal.node
        wrapped : NodeValue -> NodeValue
        wrapped = |nv| NodeValue.from_str(f(NodeValue.to_i64(nv)))

        { node: SignalNode.make_map_signal(source, Box.box(wrapped)) }
    }

    ## Fold events into a signal using an accumulator (polymorphic)
    fold : a, Event(e), (a, e -> a) -> Signal(a)
        where [
            a.encode : a, NodeValue -> Try(NodeValue, []),
            a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
            e.decode : NodeValue, NodeValue -> (Try(e, [TypeMismatch]), NodeValue),
        ]
    fold = |initial, event, step_fn| {
        A : a
        E : e
        initial_result = A.encode(initial, NodeValue.format)
        initial_nv =
            match initial_result {
                Ok(nv) => nv
            }
        event_node = Event.to_node(event)
        wrapped : (NodeValue, NodeValue) -> NodeValue
        wrapped = |(acc_nv, evt_nv)| {
            (acc_result, _) = A.decode(NodeValue.format, acc_nv)
            (evt_result, _) = E.decode(NodeValue.format, evt_nv)
            acc =
                match acc_result {
                    Ok(val) => val
                    Err(_) => ...
                }
            evt =
                match evt_result {
                    Ok(val) => val
                    Err(_) => ...
                }
            new_acc = step_fn(acc, evt)
            encode_result = A.encode(new_acc, NodeValue.format)
            match encode_result {
                Ok(nv) => nv
            }
        }

        {
            node: SignalNode.make_fold(
                initial_nv,
                event_node,
                Box.box(wrapped),
            ),
        }
    }

    ## Fold events into a signal using an accumulator (I64 version)
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
            node: SignalNode.make_fold(
                NodeValue.from_i64(initial),
                event_node,
                Box.box(wrapped),
            ),
        }
    }
}
