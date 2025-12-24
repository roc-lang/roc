# META
~~~ini
description=List.last - getting the last element of a list
type=repl
~~~
# SOURCE
~~~roc
» List.last([1, 2, 3])
» List.last(["hello", "world"])
» List.last(["hello"])
» List.last([])
» List.last(List.with_capacity(10))
~~~
# OUTPUT
Ok(3)
---
Ok("world")
---
Ok("hello")
---
Err(ListWasEmpty)
---
Err(ListWasEmpty)
# PROBLEMS
NIL
