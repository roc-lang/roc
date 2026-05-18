# META
~~~ini
description=RC: Box.unbox on shared box with heap-allocated string (>23 bytes)
type=repl
~~~
# SOURCE
~~~roc
» box = Box.box(Str.concat("abcdefghijklm", "nopqrstuvwxyz"))
» Str.concat(Box.unbox(box), Box.unbox(box))
~~~
# OUTPUT
assigned `box`
---
"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
# PROBLEMS
NIL
