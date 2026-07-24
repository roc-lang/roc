# META
~~~ini
description=List.insert inserts an element at the given index
type=repl
~~~
# SOURCE
~~~roc
» [1.I64, 2, 3, 4].insert(2, 9)
» [1.I64, 2, 3].insert(0, 9)
» [1.I64, 2, 3].insert(3, 9)
» [1.I64, 2, 3].insert(5, 9)
» items = ["a heap-allocated string long enough to avoid inline storage", "another heap string that will not be stored inline either"]
» items.insert(1, "an inserted heap string that also needs a real allocation")
» items
~~~
# OUTPUT
Ok([1, 2, 9, 3, 4])
---
Ok([9, 1, 2, 3])
---
Ok([1, 2, 3, 9])
---
Err(OutOfBounds)
---
assigned `items`
---
Ok(["a heap-allocated string long enough to avoid inline storage", "an inserted heap string that also needs a real allocation", "another heap string that will not be stored inline either"])
---
["a heap-allocated string long enough to avoid inline storage", "another heap string that will not be stored inline either"]
# PROBLEMS
NIL
