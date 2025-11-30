# META
~~~ini
description=List.sublist returns a continuous subsection of the original list
type=repl
~~~
# SOURCE
~~~roc
» nested = List.sublist([[0, 1], [2, 3], [4, 5], [6]], {start: 2, len: 1})
» List.get(nested, 0)
~~~
# OUTPUT
assigned `nested`
---
Ok([4, 5])
# PROBLEMS
NIL
