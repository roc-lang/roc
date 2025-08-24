# META
~~~ini
description=Mixed destructue patterns
type=expr
~~~
# SOURCE
~~~roc
(|{ a, x: (b, c), y: { d, e }}| a + b + c + d + e )({ a: 1, x: (2, 3), y: {d: 4, e: 5}})
~~~
# TOKENS
~~~text
OpenRound OpBar OpenCurly LowerIdent Comma LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound Comma LowerIdent OpColon OpenCurly LowerIdent Comma LowerIdent CloseCurly CloseCurly OpBar LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseRound OpenRound OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon OpenRound Int Comma Int CloseRound Comma LowerIdent OpColon OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int CloseCurly CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (binop_plus
        (binop_plus
          (binop_plus
            (binop_plus
              (lc "a")
              (lc "b")
            )
            (lc "c")
          )
          (lc "d")
        )
        (lc "e")
      )
    )
    (args
      (record_literal
        (binop_colon
          (lc "a")
          (lc "a")
        )
        (binop_colon
          (tuple_literal
            (binop_colon
              (lc "x")
              (tuple_literal
                (lc "b")
                (lc "c")
              )
            )
            (lc "y")
          )
          (record_literal
            (binop_colon
              (lc "d")
              (lc "d")
            )
            (binop_colon
              (lc "e")
              (lc "e")
            )
          )
        )
      )
    )
  )
  (block
    (binop_colon
      (lc "a")
      (binop_colon
        (tuple_literal
          (binop_colon
            (tuple_literal
              (num_literal_i32 1)
              (lc "x")
            )
            (tuple_literal
              (num_literal_i32 2)
              (num_literal_i32 3)
            )
          )
          (lc "y")
        )
        (block
          (binop_colon
            (lc "d")
            (binop_colon
              (tuple_literal
                (num_literal_i32 4)
                (lc "e")
              )
              (num_literal_i32 5)
            )
          )
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:2 to 1:31

**Unsupported Node**
at 1:73 to 1:74

**Unsupported Node**
at 1:83 to 1:84

# CANONICALIZE
~~~clojure
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
(expr :tag apply_ident :type "_f")
~~~
# TYPES
~~~roc
_f
~~~
