# META
~~~ini
description=Lambda with multiple non-consecutive argument type mismatches
type=file
~~~
# SOURCE
~~~roc
module []

# Function with 8 arguments where several types must match (a appears in positions 1, 3, 5, 7)
multi_arg_fn : a, b, a, c, a, d, a, e -> (a, b, c, d, e)
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8| 
    (x1, x2, x4, x6, x8)

# Call with mismatched types - args 1, 3, 5, and 7 should all be the same type 'a'
# but we're passing U64, Str, F64, Bool which are all different
result = multi_arg_fn(
    42,        # x1: U64 (type 'a')
    "hello",   # x2: Str (type 'b') - correct
    "world",   # x3: Str (should be 'a' = U64) - MISMATCH  
    1.5,       # x4: F64 (type 'c') - correct
    3.14,      # x5: F64 (should be 'a' = U64) - MISMATCH
    [1, 2],    # x6: List I64 (type 'd') - correct
    True,      # x7: Bool (should be 'a' = U64) - MISMATCH
    "done",    # x8: Str (type 'e') - correct
)
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpColon LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent OpArrow OpenRound LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound Int Comma String Comma String Comma Float Comma Float Comma OpenSquare Int Comma Int CloseSquare Comma UpperIdent Comma String Comma CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "multi_arg_fn")
    (binop_thin_arrow
      (lc "a")
      (binop_thin_arrow
        (lc "b")
        (binop_thin_arrow
          (lc "a")
          (binop_thin_arrow
            (lc "c")
            (binop_thin_arrow
              (lc "a")
              (binop_thin_arrow
                (lc "d")
                (binop_thin_arrow
                  (lc "a")
                  (binop_thin_arrow
                    (lc "e")
                    (tuple_literal
                      (lc "a")
                      (lc "b")
                      (lc "c")
                      (lc "d")
                      (lc "e")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (binop_equals
    (lc "multi_arg_fn")
    (lambda
      (body
        (tuple_literal
          (lc "x1")
          (lc "x2")
          (lc "x4")
          (lc "x6")
          (lc "x8")
        )
      )
      (args
        (tuple_literal
          (lc "x1")
          (lc "x2")
          (lc "x3")
          (lc "x4")
          (lc "x5")
          (lc "x6")
          (lc "x7")
          (lc "x8")
        )
      )
    )
  )
  (binop_equals
    (lc "result")
    (apply_lc
      (lc "multi_arg_fn")
      (tuple_literal
        (num_literal_i32 42)
        (str_literal_big "hello")
        (str_literal_big "world")
        (frac_literal_small 1.5)
        (frac_literal_small 3.14)
        (list_literal
          (tuple_literal
            (num_literal_i32 1)
            (num_literal_i32 2)
          )
        )
        (uc "True")
        (str_literal_small "done")
        (malformed malformed:expr_unexpected_token)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []


# Function with 8 arguments where several types must match (a appears in positions 1, 3, 5, 7)
multi_arg_fn: (a -> (b -> (a -> (c -> (a -> (d -> (a -> (e -> (a, b, c, d, e)))))))))
multi_arg_fn = \(x1, x2, x3, x4, x5, x6, x7, x8) -> (x1, x2, x4, x6, x8)
result = multi_arg_fn((42, "hello", "world", 1.5, 3.14, [(1, 2)], True, "done", <malformed>))
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 19:1 to 19:1

**Parse Error**
at 10:10 to 19:2

**Unsupported Node**
at 4:16 to 4:57

**Unsupported Node**
at 5:16 to 6:5

**Unsupported Node**
at 16:5 to 16:12

**Unsupported Node**
at 19:1 to 19:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "multi_arg_fn")
    (Expr.malformed)
  )
  (Expr.malformed)
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
