# META
~~~ini
description=List.join
type=repl
~~~
# SOURCE
~~~roc
» List.join([[1, 2], [3], [], [4, 5]])
» List.join([["a", "b"], ["c"]])
» List.join([[], [], []])
» List.join([])
» lists = [["a string long enough to be heap-allocated instead of stored inline", "another string that is long enough to require a heap allocation"], ["one more heap-allocated string to exercise element refcounting"]]
» List.join(lists)
» lists
~~~
# OUTPUT
[1.0, 2.0, 3.0, 4.0, 5.0]
---
["a", "b", "c"]
---
[]
---
[]
---
assigned `lists`
---
["a string long enough to be heap-allocated instead of stored inline", "another string that is long enough to require a heap allocation", "one more heap-allocated string to exercise element refcounting"]
---
[["a string long enough to be heap-allocated instead of stored inline", "another string that is long enough to require a heap allocation"], ["one more heap-allocated string to exercise element refcounting"]]
# PROBLEMS
NIL
