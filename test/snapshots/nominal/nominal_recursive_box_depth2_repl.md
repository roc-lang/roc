# META
~~~ini
description=REPL test for recursive nominal with nested Box at depth 2+ (runtime execution test)
type=repl
~~~
# SOURCE
~~~roc
» RichDoc := [PlainText(Str), Wrapped(Box(RichDoc))]
» depth0 = RichDoc.PlainText("hello")
» depth1 = RichDoc.Wrapped(Box.box(depth0))
» depth2 = RichDoc.Wrapped(Box.box(depth1))
~~~
# OUTPUT
assigned `depth0`
---
assigned `depth1`
---
assigned `depth2`
# PROBLEMS
NIL
