# META
~~~ini
description=Unary minus and boolean not (should error)
type=expr
~~~
# SOURCE
~~~roc
-!h
~~~
# TOKENS
~~~text
OpUnaryMinus OpBang LowerIdent ~~~
# PARSE
~~~clojure
(unary_neg <unary_op>)
~~~
# FORMATTED
~~~roc
-!h
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - minus_not_h.md:1:1:1:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **h** in this scope.
Is there an **import** or **exposing** missing up-top?

**minus_not_h.md:1:3:1:4:**
```roc
-!h
```
  ^


# CANONICALIZE
~~~clojure
(Expr.unary_neg)
~~~
# SOLVED
~~~clojure
; Total type variables: 4
(var #0 _)
(var #1 _)
(var #2 -> #1)
(var #3 -> #1)
~~~
# TYPES
~~~roc
~~~
