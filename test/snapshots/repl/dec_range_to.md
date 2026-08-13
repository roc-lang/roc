# META
~~~ini
description=Dec inclusive range syntax iterates from start to end
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold((0.5.Dec..=2.5).iter(), [], |acc, item| acc.append(item))
» Iter.fold((1.25.Dec..=1.25).iter(), [], |acc, item| acc.append(item))
» Iter.fold((3.5.Dec..=2.5).iter(), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[0.5, 1.5, 2.5]
---
[1.25]
---
[]
# PROBLEMS
NIL
