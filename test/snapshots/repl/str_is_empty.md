# META
~~~ini
description=Str.is_empty should work with empty and non-empty strings
type=repl
~~~
# SOURCE
~~~roc
» Str.is_empty("")
» Str.is_empty("a")
~~~
# OUTPUT
Evaluation error: error.TypeMismatch
---
Evaluation error: error.TypeMismatch
# PROBLEMS
NIL
