# META
~~~ini
description=Deeply nested closures
type=repl
~~~
# SOURCE
~~~roc
» (((|a| |b| |c| a + b + c)(100))(20))(3)
~~~
# OUTPUT
Evaluation error: error.TypeMismatch
# PROBLEMS
NIL
