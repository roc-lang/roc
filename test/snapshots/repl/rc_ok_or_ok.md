# META
~~~ini
description=RC: ok_or on Ok Result (extracts payload)
type=repl
~~~
# SOURCE
~~~roc
» Str.from_utf8([72, 105]).ok_or("x")
~~~
# OUTPUT
"Hi"
# PROBLEMS
NIL
