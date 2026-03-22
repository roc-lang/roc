# META
~~~ini
description=List.replace - replaces an element from a list
type=repl
~~~
# SOURCE
~~~roc
» List.replace([{}, {}], 0, {})
» List.replace(["apple", "banana", "cherry"], 2, "orange")
» List.replace([10u8, 20u8, 30u8], 1, 99u8)
~~~
# PROBLEMS
NIL
