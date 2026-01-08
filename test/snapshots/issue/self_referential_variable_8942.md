# META
~~~ini
description=Regression test for stack overflow with self-referential variable (issue #8942)
type=repl
~~~
# SOURCE
~~~roc
» a = a
» a
~~~
# OUTPUT
assigned `a`
---
Crash: Compile-time error encountered at runtime
# PROBLEMS
NIL
