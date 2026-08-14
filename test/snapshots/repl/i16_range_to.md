# META
~~~ini
description=I16 inclusive range syntax iterates from start to end
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold((-2.I16..=2.I16).iter(), [], |acc, item| acc.append(item))
» Iter.fold((0.I16..=0.I16).iter(), [], |acc, item| acc.append(item))
» Iter.fold((3.I16..=1.I16).iter(), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[-2, -1, 0, 1, 2]
---
[0]
---
[]
# PROBLEMS
NIL
