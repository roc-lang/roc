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
(unary_neg <unary_op>)
~~~
# FORMATTED
~~~roc
-foo
~~~
# EXPECTED
UNDEFINED VARIABLE - unary_negation.md:1:2:1:5
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
; Total type variables: 3
(var #0 _)
(var #1 _)
(var #2 -> #1)
~~~
# TYPES
~~~roc
~~~
