# META
~~~ini
description=Iter.take_last / drop_last over a range exercise its Known length (steps_between)
type=repl
~~~
# SOURCE
~~~roc
» Iter.fold(Iter.take_last(5.I64..<10, 2), [], |acc, item| acc.append(item))
» Iter.fold(Iter.drop_last(5.I64..<10, 2), [], |acc, item| acc.append(item))
» Iter.fold(Iter.take_last(1.I64..=5, 3), [], |acc, item| acc.append(item))
» Iter.fold(Iter.drop_last(1.I64..=5, 3), [], |acc, item| acc.append(item))
~~~
# OUTPUT
[8, 9]
---
[5, 6, 7]
---
[3, 4, 5]
---
[1, 2]
# PROBLEMS
NIL
