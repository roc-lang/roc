# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
Fli/main.roc" }

Pair(a, b+ : (
~~~
# TOKENS
~~~text
UpperIdent OpSlash LowerIdent Dot LowerIdent MalformedString UpperIdent OpenRound LowerIdent Comma LowerIdent OpPlus OpColon OpenRound ~~~
# PARSE
~~~clojure
(block
  (binop_slash
    (uc "Fli")
    (binop_pipe
      (lc "main")
      (dot_lc "roc")
    )
  )
  (malformed malformed:expr_unexpected_token)
  (apply_uc
    (uc "Pair")
    (tuple_literal
      (lc "a")
      (binop_plus
        (lc "b")
        (apply_anon
          (malformed malformed:expr_unexpected_token)
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
Fli / main.roc
" }

Pair((a, b + : (()))
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:13 to 1:13

**Parse Error**
at 3:12 to 3:12

**Parse Error**
at 3:12 to 3:15

**Parse Error**
at 3:1 to 3:15

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_thin_arrow)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
