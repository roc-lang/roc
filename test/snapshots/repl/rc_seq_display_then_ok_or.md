# META
~~~ini
description=RC sequence: plain from_utf8 display then ok_or (REPL state poisoning test)
type=repl
~~~
# SOURCE
~~~roc
» Str.from_utf8([72, 105])
» Str.from_utf8([72, 105]).ok_or("x")
~~~
# OUTPUT
Ok("Hi")
---
"Hi"
# PROBLEMS
NIL
