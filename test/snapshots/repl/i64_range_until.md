# META
~~~ini
description=I64 exclusive range syntax iterates from start to end
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold((-2.I64..<2.I64).iter(), [], |acc, item| acc.append(item))
» Iter.fold((1.I64..<1.I64).iter(), [], |acc, item| acc.append(item))
» Iter.fold((3.I64..<1.I64).iter(), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[-2, -1, 0, 1]
---
[]
---
[]
# PROBLEMS
NIL
