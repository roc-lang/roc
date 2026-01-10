# META
~~~ini
description=Regression test for issue 8898: polymorphic function creating list literals with type parameter
type=repl
~~~
# SOURCE
~~~roc
» rev = |l| List.fold(l, [], |acc, e| List.concat([e], acc))
» rev(["a", "b", "c"])
~~~
# OUTPUT
assigned `rev`
---
["c", "b", "a"]
# PROBLEMS
NIL
