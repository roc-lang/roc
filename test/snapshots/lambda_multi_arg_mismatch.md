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
KwModule OpenSquare CloseSquare BlankLine LineComment LowerIdent OpColon LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent OpArrow OpenRound LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LineComment LowerIdent OpAssign LowerIdent OpenRound Int Comma LineComment String Comma LineComment String Comma LineComment Float Comma LineComment Float Comma LineComment OpenSquare Int Comma Int CloseSquare Comma LineComment UpperIdent Comma LineComment String Comma LineComment CloseRound ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

multi_arg_fn : a -> b -> a -> c -> a -> d -> a -> e -> (a, b, c, d, e)
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8| (x1, x2, x4, x6, x8)
result = multi_arg_fn((
	42,
	"hello",
	"world",
	1.5,
	3.14,
	[1, 2],
	True,
	"done",
))

# Function with 8 arguments where several types must match (a appears in positions 1, 3, 5, 7)
# Call with mismatched types - args 1, 3, 5, and 7 should all be the same type 'a'
# but we're passing U64, Str, F64, Bool which are all different
# x1: U64 (type 'a')
# x2: Str (type 'b') - correct
# x3: Str (should be 'a' = U64) - MISMATCH  
# x4: F64 (type 'c') - correct
# x5: F64 (should be 'a' = U64) - MISMATCH
# x6: List I64 (type 'd') - correct
# x7: Bool (should be 'a' = U64) - MISMATCH
# x8: Str (type 'e') - correct
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "multi_arg_fn")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.binop_thin_arrow
        (Expr.lookup "b")
        (Expr.binop_thin_arrow
          (Expr.lookup "a")
          (Expr.binop_thin_arrow
            (Expr.lookup "c")
            (Expr.binop_thin_arrow
              (Expr.lookup "a")
              (Expr.binop_thin_arrow
                (Expr.lookup "d")
                (Expr.binop_thin_arrow
                  (Expr.lookup "a")
                  (Expr.binop_thin_arrow
                    (Expr.lookup "e")
                    (Expr.tuple_literal
                      (Expr.lookup "a")
                      (Expr.lookup "b")
                      (Expr.lookup "c")
                      (Expr.lookup "d")
                      (Expr.lookup "e")
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
  (Expr.binop_equals
    (Expr.lookup "multi_arg_fn")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "result")
    (Expr.apply_ident)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_f")
~~~
# TYPES
~~~roc
multi_arg_fn : _f
result : _f
~~~
