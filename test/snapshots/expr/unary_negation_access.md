# META
~~~ini
description=unary_negation_access
type=expr
~~~
# SOURCE
~~~roc
-rec1.field
~~~
# TOKENS
~~~text
OpUnaryMinus LowerIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(unary_neg <unary_op>)
~~~
# FORMATTED
~~~roc
-rec1..field
~~~
# EXPECTED
UNDEFINED VARIABLE - unary_negation_access.md:1:2:1:6
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **rec1** in this scope.
Is there an **import** or **exposing** missing up-top?

**unary_negation_access.md:1:2:1:6:**
```roc
-rec1.field
```
 ^^^^


# CANONICALIZE
~~~clojure
(Expr.unary_neg)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 -> #3)
~~~
# TYPES
~~~roc
~~~
