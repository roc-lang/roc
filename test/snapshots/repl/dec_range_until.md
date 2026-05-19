# META
~~~ini
description=Dec.until - creates an iterator of decimals from start to end (exclusive)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(0.5.until(3.5), [], |acc, item| acc.append(item))
» Iter.fold(2.0.until(2.0), [], |acc, item| acc.append(item))
» Iter.fold(3.5.until(2.5), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[0.5, 1.5, 2.5]
---
[]
---
[]
# PROBLEMS
NIL
