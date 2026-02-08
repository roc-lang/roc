app [main] { pf: platform "./platform/main.roc" }

import pf.Signal
import pf.Event
import pf.Elem

## Counter app using signal-based UI
## User code is pure - builds a graph that the platform walks
main : {} -> Elem
main = |{}| {
    # Create event channels for buttons
    { sender: inc_send, receiver: inc_recv } = Event.channel({})
    { sender: dec_send, receiver: dec_recv } = Event.channel({})

    # Map unit events to deltas
    # Note: Unit type {} doesn't have built-in encode/decode, so we use type-specific method
    inc_delta = Event.map_unit_to_i64(inc_recv, |{}| 1)
    dec_delta = Event.map_unit_to_i64(dec_recv, |{}| -1)

    # Merge increment and decrement events
    all_deltas = Event.merge(inc_delta, dec_delta)

    # Fold events into count signal (using polymorphic fold)
    count = Signal.fold(0, all_deltas, |acc, delta| acc + delta)

    # Transform count to display string (using polymorphic map)
    count_str = Signal.map(count, |c| I64.to_str(c))

    # Build UI - try polymorphic Signal.const
    Elem.div([
        Elem.button({ on_click: dec_send, label: Signal.const("-") }),
        Elem.label(count_str),
        Elem.button({ on_click: inc_send, label: Signal.const("+") }),
    ])
}
