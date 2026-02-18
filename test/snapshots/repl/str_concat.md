# META
~~~ini
description=Str.concat should work with various string combinations
type=repl
~~~
# SOURCE
~~~roc
» Str.concat("Hello, ", "World!")
» Str.concat("", "test")
» Str.concat("test", "")
» Str.concat("", "")
~~~
# OUTPUT
"Hello, World!"
---
"test"
---
"test"
---
""
# PROBLEMS
NIL
