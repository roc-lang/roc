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
[3, 2, 1]
---
[[3, 4], [1, 2]]
---
["hello"]
---
[]
# PROBLEMS
NIL
