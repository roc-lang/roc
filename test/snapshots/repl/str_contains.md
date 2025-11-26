# META
~~~ini
description=Str.contains should work with various string combinations
type=repl
~~~
# SOURCE
~~~roc
» Str.contains("foobarbaz", "bar")
» Str.contains("apple", "orange")
» Str.contains("anything", "")
» Str.contains("hello world", "hello")
» Str.contains("hello world", "world")
» Str.contains("test", "test")
» Str.contains("", "hello")
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
True
---
True
---
False
# PROBLEMS
NIL
