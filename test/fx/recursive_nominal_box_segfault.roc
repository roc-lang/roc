app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test for issue #8816 - recursive nominal types with nested Box at depth 2+

RichDoc := [PlainText(Str), Wrapped(Box(RichDoc))].{
    text : Str -> RichDoc
    text = |s| PlainText(s)

    wrap : RichDoc -> RichDoc
    wrap = |inner| Wrapped(Box.box(inner))

    getText : RichDoc -> Str
    getText = |doc|
        match doc {
            PlainText(s) => s
            Wrapped(boxed) => RichDoc.getText(Box.unbox(boxed))
        }
}

main! = || {
    # Create depth 0 value
    depth0 = RichDoc.text("zero")

    # Create depth 1 value
    depth1 = RichDoc.wrap(RichDoc.text("one"))

    # Create depth 2 value - this was causing segfault before the fix
    depth2 = RichDoc.wrap(RichDoc.wrap(RichDoc.text("two")))

    # Extract the text from each depth level
    s0 = RichDoc.getText(depth0)
    s1 = RichDoc.getText(depth1)
    s2 = RichDoc.getText(depth2)

    Stdout.line!("depth0: ${s0}, depth1: ${s1}, depth2: ${s2}")
}
