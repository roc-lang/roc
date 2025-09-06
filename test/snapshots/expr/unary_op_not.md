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
UNDEFINED VARIABLE - unary_op_not.md:1:2:1:9
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
; Total type variables: 3
(var #0 _)
(var #1 _)
(var #2 -> #1)
~~~
# TYPES
~~~roc
~~~
