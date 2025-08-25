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
LowerIdent OpColon MalformedUnknownToken OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon OpColon LowerIdent OpenSquare OpPercent ~~~
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
modu: (((((((<malformed>: <malformed>): <malformed>): <malformed>): <malformed>): <malformed>): <malformed>): <malformed>)
le[<malformed>]
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:6 to 1:6

**Parse Error**
at 1:8 to 1:8

**Parse Error**
at 1:10 to 1:10

**Parse Error**
at 1:12 to 1:12

**Parse Error**
at 1:14 to 1:14

**Parse Error**
at 1:16 to 1:16

**Parse Error**
at 1:18 to 1:18

**Parse Error**
at 1:20 to 1:20

**Parse Error**
at 1:24 to 1:24

**Parse Error**
at 1:23 to 1:25

**Unsupported Node**
at 1:6 to 1:6

**Unsupported Node**
at 1:8 to 1:8

**Unsupported Node**
at 1:10 to 1:10

**Unsupported Node**
at 1:12 to 1:12

**Unsupported Node**
at 1:14 to 1:14

**Unsupported Node**
at 1:16 to 1:16

**Unsupported Node**
at 1:18 to 1:18

**Unsupported Node**
at 1:20 to 1:20

**Unsupported Node**
at 1:23 to 1:24

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
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
