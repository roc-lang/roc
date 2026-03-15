# META
~~~ini
description=List.rev - reverses the list
type=repl
~~~
# SOURCE
~~~roc
» List.rev([1, 2, 3])
» List.rev([[1, 2], [3, 4]])
» List.rev(["hello"])
» List.rev([])
~~~
# OUTPUT
[3.0, 2.0, 1.0]
---
[[3.0, 4.0], [1.0, 2.0]]
---
["hello"]
---
[]
# PROBLEMS
NIL
