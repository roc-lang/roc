# META
~~~ini
description=Str.reserve should return the same string with additional capacity reserved
type=repl
~~~
# SOURCE
~~~roc
» Str.reserve("hello", 0)
» Str.reserve("hello", 10)
» Str.reserve("", 100)
~~~
# OUTPUT
"hello"
---
"hello"
---
""
# PROBLEMS
NIL
