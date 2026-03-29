# META
~~~ini
description=RC: Box.box then Box.unbox a heap string
type=repl
~~~
# SOURCE
~~~roc
» Box.unbox(Box.box(Str.concat("hel", "lo")))
~~~
# OUTPUT
"hello"
# PROBLEMS
NIL
