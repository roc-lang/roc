# META
~~~ini
description=Polymorphic comparison with numeric literals inside lambda
type=repl
~~~
# SOURCE
~~~roc
» is_positive = |x| x > 0
» List.any([1, 0, -1], is_positive)
~~~
# OUTPUT
assigned `is_positive`
---
True
# PROBLEMS
NIL
