# META
~~~ini
description=Str.from_utf8_lossy should convert a list of UTF-8 bytes to a string
type=repl
~~~
# SOURCE
~~~roc
» Str.from_utf8_lossy(Str.to_utf8(""))
» Str.from_utf8_lossy(Str.to_utf8("hello"))
» Str.from_utf8_lossy(Str.to_utf8("hello world"))
» Str.from_utf8_lossy(Str.to_utf8("é"))
» Str.from_utf8_lossy(Str.to_utf8("🎉"))
~~~
# OUTPUT
""
---
"hello"
---
"hello world"
---
"é"
---
"🎉"
# PROBLEMS
NIL
