# META
~~~ini
description=Str.trim_end should work with various string combinations
type=repl
~~~
# SOURCE
~~~roc
» Str.trim_end("  Hello")
» Str.trim_end("Hello  ")
» Str.trim_end("  Hello World  ")
» Str.trim_end("Hello World")
» Str.trim_end("    ")
» Str.trim_end("")
~~~
# OUTPUT
"  Hello"
---
"Hello"
---
"  Hello World"
---
"Hello World"
---
""
---
""
# PROBLEMS
NIL
