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
modu :  :  :  :  :  :  :  : 
le[]
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

# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
