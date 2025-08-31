# META
~~~ini
description=unary_negation
type=expr
~~~
# SOURCE
~~~roc
-foo
~~~
# TOKENS
~~~text
OpUnaryMinus LowerIdent ~~~
# PARSE
~~~clojure
(unary_neg <unary>)
~~~
# FORMATTED
~~~roc
-foo
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **foo** in this scope.
Is there an **import** or **exposing** missing up-top?

**unary_negation.md:1:2:1:5:**
```roc
-foo
```
 ^^^


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
