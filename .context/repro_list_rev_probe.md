# META
~~~ini
description=temporary list rev probe
type=repl
~~~
# SOURCE
~~~roc
» List.len(List.rev([[1, 2], [3, 4]]))
» List.len(List.first(List.rev([[1, 2], [3, 4]])))
» List.first(List.first(List.rev([[1, 2], [3, 4]])))
~~~
# OUTPUT
2.0
---
2.0
---
3.0
# PROBLEMS
NIL
