# META
~~~ini
description=F32 inclusive range syntax iterates from start to end
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold((-2.0.F32..=2.0.F32).iter(), [], |acc, item| acc.append(item))
» Iter.fold((0.0.F32..=0.0.F32).iter(), [], |acc, item| acc.append(item))
» Iter.fold((3.0.F32..=1.0.F32).iter(), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[-2, -1, 0, 1, 2]
---
[0]
---
[]
# PROBLEMS
NIL
