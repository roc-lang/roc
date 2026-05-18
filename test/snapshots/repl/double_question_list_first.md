# META
~~~ini
description=Double question operator with List.first
type=repl
~~~
# SOURCE
~~~roc
» List.first([1, 2, 3]) ?? 0
» List.first([]) ?? 99
~~~
# OUTPUT
1.0
---
99.0
# PROBLEMS
NIL
