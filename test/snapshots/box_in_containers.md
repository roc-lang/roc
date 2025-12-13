# META
~~~ini
description=Box values inside lists, records, and tag unions
type=repl
~~~
# SOURCE
~~~roc
» [Box.box(1), Box.box(2), Box.box(3)]
» { value: Box.box("hello"), count: 5 }
» Ok(Box.box(100))
~~~
# OUTPUT
[Box(1), Box(2), Box(3)]
---
{ count: 5, value: Box("hello") }
---
Ok(Box(100))
# PROBLEMS
NIL
