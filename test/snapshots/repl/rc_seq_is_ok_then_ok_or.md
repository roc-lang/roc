# META
~~~ini
description=RC sequence: is_ok then ok_or (REPL state poisoning test)
type=repl
~~~
# SOURCE
~~~roc
» Str.from_utf8([72, 105]).is_ok()
» Str.from_utf8([72, 105]).ok_or("x")
~~~
# OUTPUT
True
---
"Hi"
# PROBLEMS
NIL
