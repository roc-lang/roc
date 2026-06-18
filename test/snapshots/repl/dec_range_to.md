# META
~~~ini
description=Dec.to - creates an iterator of decimals from start to end (inclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(0.5.Dec..=2.5, [], |acc, item| acc.append(item))
» Iter.fold(1.25.Dec..=1.25, [], |acc, item| acc.append(item))
» Iter.fold(3.5.Dec..=2.5, [], |acc, item| acc.append(item))
~~~
# OUTPUT
[0.5, 1.5, 2.5]
---
[1.25]
---
[]
# PROBLEMS
NIL
