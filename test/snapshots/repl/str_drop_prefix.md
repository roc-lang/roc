# META
~~~ini
description=Str.drop_prefix should remove prefix from string if it matches
type=repl
~~~
# SOURCE
~~~roc
» Str.drop_prefix("hello world", "hello ")
» Str.drop_prefix("hello world", "goodbye ")
» Str.drop_prefix("hello", "")
» Str.drop_prefix("hello", "hello")
» Str.drop_prefix("hi", "hello")
~~~
# OUTPUT
"world"
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
