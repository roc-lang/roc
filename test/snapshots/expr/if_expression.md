# META
~~~ini
description=If expression with conditional
type=expr
~~~
# SOURCE
~~~roc
if x > 5 "big" else "small"
~~~
# TOKENS
~~~text
KwIf LowerIdent OpGreaterThan Int String KwElse String ~~~
# PARSE
~~~clojure
(if_else
  (condition     (binop_gt
      (lc "x")
      (num_literal_i32 5)
    )
)
  (then     (str_literal_small "big")
)
  (else     (str_literal_big "small")
))
~~~
# FORMATTED
~~~roc
if x > 5 "big" else "small"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**if_expression.md:1:4:1:5:**
```roc
if x > 5 "big" else "small"
```
   ^


# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
(expr :tag if_else :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
