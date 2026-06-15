import NodeValue exposing [NodeValue]
import EventNode exposing [EventNode]
import Host

## Internal representation of a signal node (type-erased).
## Closure types are inlined to avoid type alias visibility issues.
SignalNode := [
    Const(NodeValue),
    MapSignal({ source : SignalNode, transform : Box((NodeValue -> NodeValue)) }),
    Hold({ initial : NodeValue, event : EventNode }),
    Fold({ initial : NodeValue, event : EventNode, step : Box(((NodeValue, NodeValue) -> NodeValue)) }),
    ZipWith({ source : SignalNode, event : EventNode, combine : Box(((NodeValue, NodeValue) -> NodeValue)) }),
].{
    ## Create SignalNode from Const
    make_const : NodeValue -> SignalNode
    make_const = |nv| Const(nv)

    ## Create SignalNode from MapSignal
    make_map_signal : SignalNode, Box((NodeValue -> NodeValue)) -> SignalNode
    make_map_signal = |source, transform| MapSignal({ source, transform })

    ## Create SignalNode from Fold
    make_fold : NodeValue, EventNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> SignalNode
    make_fold = |initial, event, step| Fold({ initial, event, step })

    ## Create SignalNode from ZipWith
    make_zip_with : SignalNode, EventNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> SignalNode
    make_zip_with = |source, event, combine| ZipWith({ source, event, combine })

    ## Create SignalNode from Hold
    make_hold : NodeValue, EventNode -> SignalNode
    make_hold = |initial, event| Hold({ initial, event })

    ## Walk the signal graph, creating host nodes
    ## Returns a NodeId (U64) from the host
    walk! : SignalNode => U64
    walk! = |signal| {
        match signal {
            Const(nv) =>
                Host.create_signal_const!(nv)

            MapSignal({ source, transform }) => {
                source_id = SignalNode.walk!(source)
                Host.create_signal_map!(source_id, transform)
            }

            Hold({ initial, event }) => {
                event_id = EventNode.walk!(event)
                Host.create_signal_hold!(initial, event_id)
            }

            Fold({ initial, event, step }) => {
                event_id = EventNode.walk!(event)
                Host.create_signal_fold!(initial, event_id, step)
            }

            ZipWith({ source, event, combine }) => {
                source_id = SignalNode.walk!(source)
                event_id = EventNode.walk!(event)
                Host.create_signal_zip_with!(source_id, event_id, combine)
            }
        }
    }
}
