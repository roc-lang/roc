import NodeValue exposing [NodeValue]
import Host

## Internal representation of an event node (type-erased).
## Closure types are inlined to avoid type alias visibility issues.
EventNode := [
    Source,
    MapEvent({ source : EventNode, transform : Box((NodeValue -> NodeValue)) }),
    Filter({ source : EventNode, predicate : Box((NodeValue -> Bool)) }),
    Merge({ left : EventNode, right : EventNode }),
].{
    ## Create EventNode Source
    make_source : {} -> EventNode
    make_source = |{}| Source

    ## Create EventNode from MapEvent
    make_map_event : EventNode, Box((NodeValue -> NodeValue)) -> EventNode
    make_map_event = |source, transform| MapEvent({ source, transform })

    ## Create EventNode from Filter
    make_filter : EventNode, Box((NodeValue -> Bool)) -> EventNode
    make_filter = |source, predicate| Filter({ source, predicate })

    ## Create EventNode from Merge
    make_merge : EventNode, EventNode -> EventNode
    make_merge = |left, right| Merge({ left, right })

    ## Walk the event graph, creating host nodes
    ## Returns a NodeId (U64) from the host
    walk! : EventNode => U64
    walk! = |event| {
        match event {
            Source =>
                Host.create_event_source!({})

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
        }
    }
}
