# META
~~~ini
description=U64 inclusive range syntax iterates from start to end
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold((1.U64..=5.U64).iter(), [], |acc, item| acc.append(item))
» Iter.fold((0.U64..=0.U64).iter(), [], |acc, item| acc.append(item))
» Iter.fold((5.U64..=3.U64).iter(), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[1, 2, 3, 4, 5]
---
[0]
---
[]
# PROBLEMS
NIL
