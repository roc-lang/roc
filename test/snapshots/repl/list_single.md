# META
~~~ini
description=List.single - creates a single element list, useful for pipes
type=repl
~~~
# SOURCE
~~~roc
» List.single(1)
» List.single("hello")
» List.single([1, 2, 3])
~~~
# OUTPUT
[1.0]
---
["hello"]
---
[[1.0, 2.0, 3.0]]
# PROBLEMS
NIL
