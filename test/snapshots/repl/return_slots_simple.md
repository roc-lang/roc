# META
~~~ini
description=Simple test for return slots debugging
type=repl
~~~
# SOURCE
~~~roc
» (|x| x)(5)
» (|x| x + 1)(5)
~~~
# OUTPUT
Evaluation error: error.NotImplemented
---
6
# PROBLEMS
NIL
