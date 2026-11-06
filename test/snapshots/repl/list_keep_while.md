# META
~~~ini
description=List.keep_while keeps the items before the first one the predicate rejects
type=repl
~~~
# SOURCE
~~~roc
» List.keep_while([1, 2, 3], |item| item < 3)
» List.keep_while([1, 2, 3], |_| Bool.False)
» List.keep_while([1, 2, 3], |_| Bool.True)
» List.keep_while([1, 5, 2], |item| item < 3)
» List.keep_while([], |_| Bool.True)
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
