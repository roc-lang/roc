# META
~~~ini
description=RC: Box.unbox on shared box with heap string
type=repl
~~~
# SOURCE
~~~roc
» box = Box.box(Str.concat("hel", "lo"))
» Str.concat(Box.unbox(box), Box.unbox(box))
~~~
# OUTPUT
assigned `box`
---
"hellohello"
# PROBLEMS
NIL
