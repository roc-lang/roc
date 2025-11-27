# META
~~~ini
description=Str.to_utf8 should convert a string to a list of UTF-8 bytes
type=repl
~~~
# SOURCE
~~~roc
» List.len(Str.to_utf8(""))
» List.len(Str.to_utf8("hello"))
» List.len(Str.to_utf8("é"))
» List.len(Str.to_utf8("🎉"))
~~~
# OUTPUT
0
---
5
---
2
---
4
# PROBLEMS
NIL
