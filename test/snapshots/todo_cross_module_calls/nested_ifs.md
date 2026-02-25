# META
~~~ini
description=Test evaluation of nested if expressions
type=repl
~~~
# SOURCE
~~~roc
» if 5 > 3 (if 1 > 2 3 else 4) else 5
~~~
# OUTPUT
4.0
# PROBLEMS
NIL
