# META
~~~ini
description=List.first - getting the first element of a list
type=repl
~~~
# SOURCE
~~~roc
» List.get([1, 2, 3], 0)
» List.get(["hello", "world"], 1)
» List.get([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 9)
» List.get(["hello"], 1)
» List.get([], 0)
» List.get(List.with_capacity(10), 5)
~~~
# OUTPUT
Ok(1)
---
Ok("world")
---
Ok(10)
---
Err(OutOfBounds)
---
Err(OutOfBounds)
---
Err(OutOfBounds)
# PROBLEMS
NIL
