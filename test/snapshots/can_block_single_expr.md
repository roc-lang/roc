# META
~~~ini
description=Block with single expression
type=expr
~~~
# SOURCE
~~~roc
{ x }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (lc "x")
)
~~~
# FORMATTED
~~~roc
x
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_block_single_expr.md:1:3:1:4:**
```roc
{ x }
```
  ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lookup "x")
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
