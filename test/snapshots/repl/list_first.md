# META
~~~ini
description=List.first - getting the first element of a list
type=repl
~~~
# SOURCE
~~~roc
» List.first([1, 2, 3])
» List.first(["hello", "world"])
» List.first(["hello"])
» List.first([])
» List.first(List.with_capacity(10))
~~~
# OUTPUT
Ok(1)
---
Ok("hello")
---
Ok("hello")
---
Err(ListWasEmpty)
---
Err(ListWasEmpty)
# PROBLEMS
NIL
