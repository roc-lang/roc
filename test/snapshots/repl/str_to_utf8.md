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
» List.len(Str.to_utf8("Hello, World!"))
» List.len(Str.to_utf8("日本語"))
» List.len(Str.to_utf8("a é 🎉"))
» Str.from_utf8_lossy(Str.to_utf8("hello"))
» Str.from_utf8_lossy(Str.to_utf8(""))
» Str.from_utf8_lossy(Str.to_utf8("🎉 party!"))
» Str.from_utf8_lossy(Str.to_utf8("abc123"))
» List.is_empty(Str.to_utf8(""))
» List.is_empty(Str.to_utf8("x"))
~~~
# OUTPUT
0
---
5
---
2
---
4
---
13
---
9
---
9
---
"hello"
---
""
---
"🎉 party!"
---
"abc123"
---
True
---
False
# PROBLEMS
NIL
