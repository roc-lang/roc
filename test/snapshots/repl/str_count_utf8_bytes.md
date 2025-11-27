# META
~~~ini
description=Str.count_utf8_bytes should return the number of bytes in the string
type=repl
~~~
# SOURCE
~~~roc
» Str.count_utf8_bytes("")
» Str.count_utf8_bytes("hello")
» Str.count_utf8_bytes("hello world")
» Str.count_utf8_bytes("é")
» Str.count_utf8_bytes("🎉")
~~~
# OUTPUT
0
---
5
---
11
---
2
---
4
# PROBLEMS
NIL
