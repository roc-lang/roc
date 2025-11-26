# META
~~~ini
description=Str.repeat should repeat a string a given number of times
type=repl
~~~
# SOURCE
~~~roc
» Str.repeat("ab", 3)
» Str.repeat("hello", 0)
» Str.repeat("hello", 1)
» Str.repeat("", 5)
~~~
# OUTPUT
"ababab"
---
""
---
"hello"
---
""
# PROBLEMS
NIL
