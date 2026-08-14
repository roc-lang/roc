# META
~~~ini
description=U16 inclusive range syntax iterates from start to end
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold((1.U16..=5.U16).iter(), [], |acc, item| acc.append(item))
» Iter.fold((0.U16..=0.U16).iter(), [], |acc, item| acc.append(item))
» Iter.fold((5.U16..=3.U16).iter(), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL
