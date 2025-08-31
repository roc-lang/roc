# META
~~~ini
description=Unary not operation expression
type=expr
~~~
# SOURCE
~~~roc
!isValid
~~~
# TOKENS
~~~text
OpBang LowerIdent ~~~
# PARSE
~~~clojure
(unary_not <unary_op>)
~~~
# FORMATTED
~~~roc
!isValid
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **isValid** in this scope.
Is there an **import** or **exposing** missing up-top?

**unary_op_not.md:1:2:1:9:**
```roc
!isValid
```
 ^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.unary_not)
~~~
# SOLVED
~~~clojure
(expr :tag unary_not :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~
