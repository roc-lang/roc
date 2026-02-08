platform ""
    requires {
        main : {} -> Elem
    }
    exposes [Signal, Event, EventSender, Elem, NodeValue, SignalNode, EventNode]
    packages {}
    provides {
        main_for_host!: "main",
        call_transform: "call_transform",
        call_step: "call_step",
        call_predicate: "call_predicate",
    }
    targets: {
        files: "targets/",
        exe: {
            arm64mac: ["libhost.a", app],
        }
    }

import Elem exposing [Elem]
import Signal
import Event
import EventSender
import NodeValue exposing [NodeValue]
import SignalNode
import EventNode
import Host

## Called by host at startup to build the UI
main_for_host! : {} => {}
main_for_host! = |{}| {
    elem = main({})
    root = Host.create_root!({})
    Elem.walk!(elem, root)
}

## Called by host to evaluate a transform closure (for map operations)
call_transform : Box((NodeValue -> NodeValue)), NodeValue -> NodeValue
call_transform = |boxed_fn, input| {
    fn = Box.unbox(boxed_fn)
    fn(input)
}

## Called by host to evaluate a step closure (for fold, zip_with)
call_step : Box(((NodeValue, NodeValue) -> NodeValue)), NodeValue, NodeValue -> NodeValue
call_step = |boxed_step, acc, event_val| {
    step = Box.unbox(boxed_step)
    step((acc, event_val))
}

## Called by host to evaluate a predicate closure (for filter)
call_predicate : Box((NodeValue -> Bool)), NodeValue -> Bool
call_predicate = |boxed_pred, input| {
    pred = Box.unbox(boxed_pred)
    pred(input)
}
