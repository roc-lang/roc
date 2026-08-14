# META
~~~ini
description=U16 exclusive range syntax iterates from start to end
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold((0.U16..<3.U16).iter(), [], |acc, item| acc.append(item))
» Iter.fold((1.U16..<1.U16).iter(), [], |acc, item| acc.append(item))
» Iter.fold((5.U16..<3.U16).iter(), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[0, 1, 2]
---
[]
---
[]
# PROBLEMS
NIL
