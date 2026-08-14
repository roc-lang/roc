# META
~~~ini
description=U32 exclusive range syntax iterates from start to end
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold((0.U32..<3.U32).iter(), [], |acc, item| acc.append(item))
» Iter.fold((1.U32..<1.U32).iter(), [], |acc, item| acc.append(item))
» Iter.fold((5.U32..<3.U32).iter(), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[0, 1, 2]
---
[]
---
[]
# PROBLEMS
NIL
