import NodeValue exposing [NodeValue]
import Host

## Internal type-erased graph nodes.
## SignalNode and EventNode live under one wrapper module so they can reference
## each other without creating Roc module import cycles.
Graph := [].{
    EventNode := [
        Source,
        Prebuilt(U64),
        MapEvent({ source : EventNode, transform : Box((NodeValue -> NodeValue)) }),
        Filter({ source : EventNode, predicate : Box((NodeValue -> Bool)) }),
        Merge({ left : EventNode, right : EventNode }),
        WithLatest({ event : EventNode, signal : SignalNode, combine : Box(((NodeValue, NodeValue) -> NodeValue)) }),
    ].{
        make_prebuilt : U64 -> EventNode
        make_prebuilt = |id| Prebuilt(id)

        make_map_event : EventNode, Box((NodeValue -> NodeValue)) -> EventNode
        make_map_event = |source, transform| MapEvent({ source, transform })

        make_filter : EventNode, Box((NodeValue -> Bool)) -> EventNode
        make_filter = |source, predicate| Filter({ source, predicate })

        make_merge : EventNode, EventNode -> EventNode
        make_merge = |left, right| Merge({ left, right })

        make_with_latest : EventNode, SignalNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> EventNode
        make_with_latest = |event, signal, combine| WithLatest({ event, signal, combine })

        walk! : EventNode => U64
        walk! = |event| {
            match event {
                Source =>
                    Host.create_event_source!()

                Prebuilt(id) =>
                    id

                MapEvent({ source, transform }) => {
                    source_id = EventNode.walk!(source)
                    Host.create_event_map!(source_id, transform)
                }

                Filter({ source, predicate }) => {
                    source_id = EventNode.walk!(source)
                    Host.create_event_filter!(source_id, predicate)
                }

                Merge({ left, right }) => {
                    left_id = EventNode.walk!(left)
                    right_id = EventNode.walk!(right)
                    Host.create_event_merge!(left_id, right_id)
                }

                WithLatest({ event: source_event, signal, combine }) => {
                    event_id = EventNode.walk!(source_event)
                    signal_id = SignalNode.walk!(signal)
                    Host.create_event_with_latest!(event_id, signal_id, combine)
                }
            }
        }
    }

    SignalNode := [
        ConstSignal(NodeValue),
        State(NodeValue),
        Prebuilt(U64),
        MapSignal({ source : SignalNode, transform : Box((NodeValue -> NodeValue)) }),
        Map2Signal({ left : SignalNode, right : SignalNode, transform : Box(((NodeValue, NodeValue) -> NodeValue)) }),
        Hold({ initial : NodeValue, event : EventNode }),
        Fold({ initial : NodeValue, event : EventNode, step : Box(((NodeValue, NodeValue) -> NodeValue)) }),
        ZipWith({ source : SignalNode, event : EventNode, combine : Box(((NodeValue, NodeValue) -> NodeValue)) }),
    ].{
        make_const : NodeValue -> SignalNode
        make_const = |nv| ConstSignal(nv)

        make_state : NodeValue -> SignalNode
        make_state = |initial| State(initial)

        make_prebuilt_signal : U64 -> SignalNode
        make_prebuilt_signal = |id| Prebuilt(id)

        make_map_signal : SignalNode, Box((NodeValue -> NodeValue)) -> SignalNode
        make_map_signal = |source, transform| MapSignal({ source, transform })

        make_map2_signal : SignalNode, SignalNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> SignalNode
        make_map2_signal = |left, right, transform| Map2Signal({ left, right, transform })

        make_fold : NodeValue, EventNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> SignalNode
        make_fold = |initial, event, step| Fold({ initial, event, step })

        make_zip_with : SignalNode, EventNode, Box(((NodeValue, NodeValue) -> NodeValue)) -> SignalNode
        make_zip_with = |source, event, combine| ZipWith({ source, event, combine })

        make_hold : NodeValue, EventNode -> SignalNode
        make_hold = |initial, event| Hold({ initial, event })

        walk! : SignalNode => U64
        walk! = |signal| {
            match signal {
                ConstSignal(nv) =>
                    Host.create_signal_const!(nv)

                State(initial) =>
                    Host.create_signal_state!(initial)

                Prebuilt(id) =>
                    id

                MapSignal({ source, transform }) => {
                    source_id = SignalNode.walk!(source)
                    Host.create_signal_map!(source_id, transform)
                }

                Map2Signal({ left, right, transform }) => {
                    left_id = SignalNode.walk!(left)
                    right_id = SignalNode.walk!(right)
                    Host.create_signal_map2!(left_id, right_id, transform)
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
}
