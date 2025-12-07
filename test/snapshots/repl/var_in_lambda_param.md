# META
~~~ini
description=Test var in lambda parameters
type=repl
~~~
# SOURCE
~~~roc
» f = |var $x, y| { $x = $x + y
    $x }
» f(1, 2)
~~~
# OUTPUT
assigned `f`
---
3
# PROBLEMS
NIL
