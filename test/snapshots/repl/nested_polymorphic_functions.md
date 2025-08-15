# META
~~~ini
description=Nested polymorphic function calls
type=repl
~~~
# SOURCE
~~~roc
» (|identity| { a: identity(10), b: identity(20), c: identity(30) })(|x| x)
~~~
# OUTPUT
<record>
# PROBLEMS
NIL
