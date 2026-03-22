## Hosted effects - implemented by host (Zig), called by Roc.
## The host stores all state including boxed closures.
##
## NodeId (U64) identifies a node in the host's reactive graph.
## ElemId (U64) identifies a DOM element.

import NodeValue exposing [NodeValue]

## Host effects for creating DOM elements and graph nodes
Host := [].{
    ## Create the root element container
    create_root! : {} => U64

    ## Create a DOM element by tag name
    create_element! : Str => U64

    ## Set element text content
    set_text! : U64, Str => {}

    ## Append child to parent element
    append_child! : U64, U64 => {}

    ## Create an event source node in the graph
    create_event_source! : {} => U64

    ## Create an event map node (host stores the transform closure)
    create_event_map! : U64, Box((NodeValue -> NodeValue)) => U64

    ## Create an event filter node (host stores the predicate closure)
    create_event_filter! : U64, Box((NodeValue -> Bool)) => U64

    ## Create an event merge node
    create_event_merge! : U64, U64 => U64

    ## Create a constant signal node
    create_signal_const! : NodeValue => U64

    ## Create a signal map node (host stores the transform closure)
    create_signal_map! : U64, Box((NodeValue -> NodeValue)) => U64

    ## Create a signal hold node
    create_signal_hold! : NodeValue, U64 => U64

    ## Create a signal fold node (host stores the step closure)
    create_signal_fold! : NodeValue, U64, Box(((NodeValue, NodeValue) -> NodeValue)) => U64

    ## Create a signal zip_with node (host stores the combine closure)
    create_signal_zip_with! : U64, U64, Box(((NodeValue, NodeValue) -> NodeValue)) => U64

    ## Bind a signal to an element's text content
    bind_text! : U64, U64 => {}

    ## Bind click events from element to event node
    bind_click! : U64, U64 => {}
}
