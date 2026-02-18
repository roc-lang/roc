# META
~~~ini
description=Str.with_prefix should prepend a prefix to a string
type=repl
~~~
# SOURCE
~~~roc
» Str.with_prefix("world", "hello ")
» Str.with_prefix("hello", "")
» Str.with_prefix("", "prefix")
» Str.with_prefix("", "")
~~~
# OUTPUT
"hello world"
---
"hello"
---
"prefix"
---
""
# PROBLEMS
NIL
