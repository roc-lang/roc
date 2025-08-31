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
NIL
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
(expr :tag unary_neg :type "Num(_size)")
~~~
# TYPES
~~~roc
Num(_size)
~~~
