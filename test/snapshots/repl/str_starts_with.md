# META
~~~ini
description=Str.starts_with should check if string starts with prefix
type=repl
~~~
# SOURCE
~~~roc
» Str.starts_with("hello world", "hello")
» Str.starts_with("hello world", "world")
» Str.starts_with("", "")
» Str.starts_with("hello", "")
» Str.starts_with("hi", "hello")
~~~
# OUTPUT
True
---
False
---
True
---
True
---
False
# PROBLEMS
NIL
