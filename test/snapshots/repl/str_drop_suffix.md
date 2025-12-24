# META
~~~ini
description=Str.drop_suffix should remove suffix from string if it matches
type=repl
~~~
# SOURCE
~~~roc
» Str.drop_suffix("hello world", " world")
» Str.drop_suffix("hello world", " goodbye")
» Str.drop_suffix("hello", "")
» Str.drop_suffix("hello", "hello")
» Str.drop_suffix("hi", "hello")
~~~
# OUTPUT
"hello"
---
"hello world"
---
"hello"
---
""
---
"hi"
# PROBLEMS
NIL
