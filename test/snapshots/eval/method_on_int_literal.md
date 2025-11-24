# META
~~~ini
description=Method call directly on integer literal
type=repl
~~~
# SOURCE
~~~roc
» 35.foo()
~~~
# OUTPUT
Crash: Num.Dec does not implement foo
# PROBLEMS
NIL
