# META
~~~ini
description=Arrow syntax desugaring (arg->fn to fn(arg))
type=repl
~~~
# SOURCE
~~~roc
» fn0 = |a| a + 1
» fn1 = |a, b| a + b
» fn2 = |a, b, c| a + b + c
» fn3 = |a, b, c, d| a + b + c + d
» 10->fn0
» 10->fn1(20)
» 10->fn2(20, 30)
» 10->fn3(20, 30, 40)
~~~
# OUTPUT
assigned `fn0`
---
assigned `fn1`
---
assigned `fn2`
---
assigned `fn3`
---
11
---
30
---
60
---
100
# PROBLEMS
NIL
