# META
~~~ini
description=U32 inclusive range syntax iterates from start to end
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold((1.U32..=5.U32).iter(), [], |acc, item| acc.append(item))
» Iter.fold((0.U32..=0.U32).iter(), [], |acc, item| acc.append(item))
» Iter.fold((5.U32..=3.U32).iter(), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL
