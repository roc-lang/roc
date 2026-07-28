# META
~~~ini
description=List.drop_swap removes an element by swapping it with the last one
type=repl
~~~
# SOURCE
~~~roc
» [1.I64, 2, 3, 4, 5].drop_swap(1)
» [1.I64, 2, 3].drop_swap(2)
» [1.I64, 2, 3].drop_swap(9)
» heap = ["one long heap-allocated string that will not fit inline", "two long heap-allocated string that will not fit inline", "three long heap-allocated string that will not fit inline"]
» heap.drop_swap(0)
» heap
~~~
# OUTPUT
[1, 5, 3, 4]
---
[1, 2]
---
[1, 2, 3]
---
assigned `heap`
---
["three long heap-allocated string that will not fit inline", "two long heap-allocated string that will not fit inline"]
---
["one long heap-allocated string that will not fit inline", "two long heap-allocated string that will not fit inline", "three long heap-allocated string that will not fit inline"]
# PROBLEMS
NIL
