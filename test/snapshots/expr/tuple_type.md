# META
~~~ini
description=tuple_type
type=expr
~~~
# SOURCE
~~~roc
{
    f : (Str, Str) -> (Str, Str)
    f = |x| x

    f((1, 2))
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon OpenRound UpperIdent Comma UpperIdent CloseRound OpArrow OpenRound UpperIdent Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpenRound OpenRound Int Comma Int CloseRound CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "f")
    (tuple_literal
      (uc "Str")
      (uc "Str")
    )
  )
  (apply_anon
    (malformed malformed:expr_unexpected_token)
    (tuple_literal
      (uc "Str")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "f")
    (lambda
      (body
        (lc "x")
      )
      (args
        (lc "x")
      )
    )
  )
  (apply_lc
    (lc "f")
    (tuple_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
    )
  )
)
~~~
# FORMATTED
~~~roc
f: (Str, Str)
-> (((Str, Str))
f = \x -> x

f((1, 2))
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:20 to 2:20

**Unsupported Node**
at 2:18 to 2:19

**Unsupported Node**
at 2:20 to 2:20

**Unsupported Node**
at 3:9 to 3:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "f")
    (Expr.malformed)
  )
  (Expr.apply_ident)
  (Expr.malformed)
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
f : Error
~~~
