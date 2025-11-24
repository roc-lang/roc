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
**UNEXPECTED TOKEN IN EXPRESSION**
The token **return** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
# PROBLEMS
NIL
