# META
~~~ini
description=Str.with_ascii_lowercased should work with various strings
type=repl
~~~
# SOURCE
~~~roc
» Str.with_ascii_lowercased("HeLLo")
» Str.with_ascii_lowercased("HELLO")
» Str.with_ascii_lowercased("hello")
» Str.with_ascii_lowercased("")
» Str.with_ascii_lowercased("COFFÉ")
~~~
# OUTPUT
"hello"
---
"hello"
---
"hello"
---
""
---
"coffÉ"
# PROBLEMS
NIL
