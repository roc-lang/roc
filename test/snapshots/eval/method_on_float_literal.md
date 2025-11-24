# META
~~~ini
description=Method call directly on float literal
type=repl
~~~
# SOURCE
~~~roc
» 12.34.foo()
~~~
# OUTPUT
Crash: Num.Dec does not implement foo
# PROBLEMS
NIL
