# META
~~~ini
description=Str.with_ascii_uppercased should work with various strings
type=repl
~~~
# SOURCE
~~~roc
» Str.with_ascii_uppercased("HeLLo")
» Str.with_ascii_uppercased("hello")
» Str.with_ascii_uppercased("HELLO")
» Str.with_ascii_uppercased("")
» Str.with_ascii_uppercased("coffÉ")
~~~
# OUTPUT
"HELLO"
---
"HELLO"
---
"HELLO"
---
""
---
"COFFÉ"
# PROBLEMS
NIL
