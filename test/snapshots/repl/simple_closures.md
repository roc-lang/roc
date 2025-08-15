# META
~~~ini
description=Simple closures
type=repl
~~~
# SOURCE
~~~roc
» (|s| s)("Test")
» (|x| x)(42)
» (|x| !x)(True)
~~~
# OUTPUT
Evaluation error: error.NotImplemented
---
Evaluation error: error.NotImplemented
---
False
# PROBLEMS
NIL
