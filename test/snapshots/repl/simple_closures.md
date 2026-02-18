# META
~~~ini
description=Simple closures
type=repl
~~~
# SOURCE
~~~roc
» (|s| s)("Test")
» (|x| x)(42)
» (|x| !x)(Bool.True)
~~~
# OUTPUT
"Test"
---
42
---
False
# PROBLEMS
NIL
