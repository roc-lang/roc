app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.NodeA
import pf.NodeB
import pf.Element

main! = || {
    Stdout.line!("Test 1: Text elements in list (should work)")
    text_elem = Element.div([
        Element.text("Hello"),
        Element.text("World"),
    ])
    Element.process!(text_elem)

    Stdout.line!("")
    Stdout.line!("Test 2: Label element with opaque payload in list")
    event = NodeA.source({})
    signal = NodeB.fold("initial", event)
    label_elem = Element.div([
        Element.label(signal),
    ])
    Element.process!(label_elem)

    Stdout.line!("Done!")
}
