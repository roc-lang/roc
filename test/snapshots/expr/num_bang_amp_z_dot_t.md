# META
~~~ini
description=num_bang_amp_z_dot_t
type=expr
~~~
# SOURCE
~~~roc
4
!
&z.t
~~~
# TOKENS
~~~text
Int OpBang OpAmpersand LowerIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
!
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **!
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**num_bang_amp_z_dot_t.md:2:1:3:1:**
```roc
!
&z.t
```


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
(expr :tag malformed :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
