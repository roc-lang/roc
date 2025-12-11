# META
~~~ini
description=Box.box and Box.unbox round-trip with record
type=repl
~~~
# SOURCE
~~~roc
» Box.unbox(Box.box({ x: 1, y: 2 }))
~~~
# OUTPUT
{ x: 1, y: 2 }
# PROBLEMS
NIL
