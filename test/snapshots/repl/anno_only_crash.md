# META
~~~ini
description=e_anno_only should crash when value is used
type=repl
~~~
# SOURCE
~~~roc
» foo : Str -> Str
» foo("test")
~~~
# OUTPUT
Crash: Compile-time error encountered at runtime
---
Crash: Cannot call function: compile-time error in function definition
# PROBLEMS
NIL
