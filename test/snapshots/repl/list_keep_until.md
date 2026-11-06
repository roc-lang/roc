# META
~~~ini
description=List.keep_until keeps the items before the first one the predicate matches
type=repl
~~~
# SOURCE
~~~roc
» List.keep_until([1, 2, 3], |item| item > 2)
» List.keep_until([1, 2, 3], |_| Bool.True)
» List.keep_until([1, 2, 3], |_| Bool.False)
» List.keep_until([1, 5, 2], |item| item > 3)
» List.keep_until([], |_| Bool.False)
~~~
# OUTPUT
[1.0, 2.0]
---
[]
---
[1.0, 2.0, 3.0]
---
[1.0]
---
[]
# PROBLEMS
NIL
