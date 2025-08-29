# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
modu:;::::::::::::::le[%
~~~
# TOKENS
~~~text
LowerIdent OpColon MalformedUnknownToken OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon LowerIdent OpenSquare MalformedUnknownToken ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "modu")
    (binop_colon
      (binop_colon
        (binop_colon
          (binop_colon
            (binop_colon
              (binop_colon
                (binop_colon
                  (malformed malformed:expr_unexpected_token)
                  (malformed malformed:expr_unexpected_token)
                )
                (malformed malformed:expr_unexpected_token)
              )
              (malformed malformed:expr_unexpected_token)
            )
            (malformed malformed:expr_unexpected_token)
          )
          (malformed malformed:expr_unexpected_token)
        )
        (malformed malformed:expr_unexpected_token)
      )
      (malformed malformed:expr_unexpected_token)
    )
  )
  (lc "le")
  (list_literal
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
modu : ; : : : : : : : : : : : : : :
le[%]
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:6 to 1:7

**Parse Error**
at 1:8 to 1:9

**Parse Error**
at 1:10 to 1:11

**Parse Error**
at 1:12 to 1:13

**Parse Error**
at 1:14 to 1:15

**Parse Error**
at 1:16 to 1:17

**Parse Error**
at 1:18 to 1:19

**Parse Error**
at 1:20 to 1:21

**Parse Error**
at 1:24 to 1:25

**Parse Error**
at 1:23 to 1:25

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "modu")
    (Expr.binop_colon
      (Expr.binop_colon
        (Expr.binop_colon
          (Expr.binop_colon
            (Expr.binop_colon
              (Expr.binop_colon
                (Expr.binop_colon
                  (Expr.malformed)
                  (Expr.malformed)
                )
                (Expr.malformed)
              )
              (Expr.malformed)
            )
            (Expr.malformed)
          )
          (Expr.malformed)
        )
        (Expr.malformed)
      )
      (Expr.malformed)
    )
  )
  (Expr.lookup "le")
  (Expr.list_literal)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
