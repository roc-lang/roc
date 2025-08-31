# META
~~~ini
description=Backslash is not valid lambda syntax - helpful error
type=expr
~~~
# SOURCE
~~~roc
\x -> x + 1
~~~
# TOKENS
~~~text
OpBackslash LowerIdent OpArrow LowerIdent OpPlus Int ~~~
# PARSE
~~~clojure
(binop_thin_arrow
  (malformed malformed:backslash_not_valid_lambda_syntax)
  (binop_plus
    (lc "x")
    (num_literal_i32 1)
  )
)
~~~
# FORMATTED
~~~roc
x -> x + 1
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **backslash_not_valid_lambda_syntax**
This is an unexpected parsing error. Please check your syntax.

**parse_error_backslash_lambda.md:1:2:1:4:**
```roc
\x -> x + 1
```
 ^^


**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**parse_error_backslash_lambda.md:1:7:1:8:**
```roc
\x -> x + 1
```
      ^


# CANONICALIZE
~~~clojure
(Expr.binop_thin_arrow
  (Expr.malformed)
  (Expr.binop_plus
    (Expr.lookup "x")
    (Expr.num_literal_i32 1)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag binop_thin_arrow :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
