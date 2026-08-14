# META
~~~ini
description=F64 exclusive range syntax iterates from start to end
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold((0.5.F64..<3.5.F64).iter(), [], |acc, item| acc.append(item))
» Iter.fold((2.0.F64..<2.0.F64).iter(), [], |acc, item| acc.append(item))
» Iter.fold((3.5.F64..<2.5.F64).iter(), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[0.5, 1.5, 2.5]
---
[]
---
[]
# PROBLEMS
NIL
