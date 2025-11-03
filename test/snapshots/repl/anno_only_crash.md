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

---
Canonicalize expr error: expression returned null for apply
# PROBLEMS
NIL
