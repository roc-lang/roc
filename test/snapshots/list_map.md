# META
~~~ini
description=List.map
type=repl
~~~
# SOURCE
~~~roc
» List.map([2, 4, 6], |val| val * 2)
» List.map([], |_| 0)
~~~
# OUTPUT
[4.0, 8.0, 12.0]
---
[]
# PROBLEMS
NIL
