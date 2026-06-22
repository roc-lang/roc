# META
~~~ini
description=repro for https://github.com/roc-lang/roc/issues/9712 — `1 % 10 // 100` must parse left-to-right as (1 % 10) // 100 = 0, not crash
type=repl
~~~
# SOURCE
~~~roc
» 1 % 10 // 100
~~~
# OUTPUT
0.0
# PROBLEMS
NIL
