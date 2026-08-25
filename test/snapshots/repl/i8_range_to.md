# META
~~~ini
description=I8 inclusive range syntax iterates from start to end
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold((-2.I8..=2.I8).iter(), [], |acc, item| acc.append(item))
» Iter.fold((0.I8..=0.I8).iter(), [], |acc, item| acc.append(item))
» Iter.fold((3.I8..=1.I8).iter(), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[-2, -1, 0, 1, 2]
---
[0]
---
[]
# PROBLEMS
NIL
