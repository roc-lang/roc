app [main!] { pf: platform "./platform/main.roc" }

import pf.Elem
import pf.Event
import pf.Signal

main! : () => {}
main! = || {
    { sender: dec_send, receiver: dec_clicks } = Event.channel!()
    { sender: inc_send, receiver: inc_clicks } = Event.channel!()

    decs = Event.map_unit_to_i64(dec_clicks, |_| -1)
    incs = Event.map_unit_to_i64(inc_clicks, |_| 1)
    changes = Event.merge(decs, incs)

    count = Signal.fold_i64(0, changes, |current, change| current + change)
    count_label = Signal.map_i64_to_str(count, |n| n.to_str())

    Elem.run!(Elem.div([
        Elem.button({ on_click: dec_send, label: Signal.const_str("-") }),
        Elem.label(count_label),
        Elem.button({ on_click: inc_send, label: Signal.const_str("+") }),
    ]))
}
