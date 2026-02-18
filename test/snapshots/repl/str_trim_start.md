# META
~~~ini
description=Str.trim_start should work with various string combinations
type=repl
~~~
# SOURCE
~~~roc
» Str.trim_start("  Hello")
» Str.trim_start("Hello  ")
» Str.trim_start("  Hello World  ")
» Str.trim_start("Hello World")
» Str.trim_start("    ")
» Str.trim_start("")
~~~
# OUTPUT
"Hello"
---
"Hello  "
---
"Hello World  "
---
"Hello World"
---
""
---
""
# PROBLEMS
NIL
