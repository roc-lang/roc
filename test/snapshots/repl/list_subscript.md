# META
~~~ini
description=List.subscript - getting an element from a list
type=repl
~~~
# SOURCE
~~~roc
» List.subscript([1, 2, 3], 0)
» List.subscript(["hello", "world"], 1)
» List.subscript([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 9)
» List.subscript(["hello"], 1)
» List.subscript([], 0)
» List.subscript(List.with_capacity(10), 5)
~~~
# OUTPUT
Ok(1.0)
---
Ok("world")
---
Ok(10.0)
---
Err(OutOfBounds)
---
Err(OutOfBounds)
---
Err(OutOfBounds)
# PROBLEMS
NIL
