# META
~~~ini
description=RC: ok_or on Err Result (uses fallback)
type=repl
~~~
# SOURCE
~~~roc
» Str.from_utf8([255]).ok_or("x")
~~~
# OUTPUT
"x"
# PROBLEMS
NIL
