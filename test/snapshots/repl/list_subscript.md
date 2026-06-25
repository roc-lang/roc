# META
~~~ini
description=List.subscript is an alias for List.get
type=repl
~~~
# SOURCE
~~~roc
» List.subscript([1, 2, 3], 0)
» List.subscript(["hello", "world"], 1)
» List.subscript(["hello"], 5)
~~~
# OUTPUT
Ok(1.0)
---
Ok("world")
---
Err(OutOfBounds)
# PROBLEMS
NIL
