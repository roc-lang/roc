# META
~~~ini
description=I32 inclusive range syntax iterates from start to end
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold((-2.I32..=2.I32).iter(), [], |acc, item| acc.append(item))
» Iter.fold((0.I32..=0.I32).iter(), [], |acc, item| acc.append(item))
» Iter.fold((3.I32..=1.I32).iter(), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[-2, -1, 0, 1, 2]
---
[0]
---
[]
# PROBLEMS
NIL
