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
[1]
---
["hello"]
---
[[1, 2, 3]]
# PROBLEMS
NIL
