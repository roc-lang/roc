# META
~~~ini
description=Return statement outside of function should be a compile error
type=repl
~~~
# SOURCE
~~~roc
» return 42
~~~
# OUTPUT
Canonicalize expr error: expression returned null
# PROBLEMS
NIL
