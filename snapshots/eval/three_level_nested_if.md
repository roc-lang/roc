# META
~~~ini
description=Test evaluation of 3-level deep nested if expressions
type=repl
~~~
# SOURCE
~~~roc
» if 10 > 5 (if 4 < 8 (if 2 == 2 100 else 200) else 300) else 400
~~~
# EXPECTED
100
# PROBLEMS
NIL
