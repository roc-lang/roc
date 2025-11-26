# META
~~~ini
description=Str.ends_with should check if string ends with suffix
type=repl
~~~
# SOURCE
~~~roc
» Str.ends_with("hello world", "world")
» Str.ends_with("hello world", "hello")
» Str.ends_with("", "")
» Str.ends_with("hello", "")
» Str.ends_with("hi", "hello")
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
