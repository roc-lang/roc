# META
~~~ini
description=Deeply nested polymorphic function calls with multiple levels
type=repl
~~~
# SOURCE
~~~roc
» (|twice, identity| { a: twice(identity, 42), b: twice(|x| x + 1, 100) })(|f, val| f(f(val)), |x| x)
~~~
# OUTPUT
<record>
# PROBLEMS
NIL
