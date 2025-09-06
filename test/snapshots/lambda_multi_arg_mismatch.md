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
(block
  (binop_colon
    (lc "multi_arg_fn")
    (binop_arrow_call
      (lc "a")
      (binop_arrow_call
        (lc "b")
        (binop_arrow_call
          (lc "a")
          (binop_arrow_call
            (lc "c")
            (binop_arrow_call
              (lc "a")
              (binop_arrow_call
                (lc "d")
                (binop_arrow_call
                  (lc "a")
                  (binop_arrow_call
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
          (num_literal_i32 1)
          (num_literal_i32 2)
        )
        (uc "True")
        (str_literal_small "done")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

# Function with 8 arguments where several types must match (a appears in positions 1, 3, 5, 7)
multi_arg_fn : a -> b -> a -> c -> a -> d -> a -> e -> (a, b, c, d, e)
multi_arg_fn = |x1, x2, x3, x4, x5, x6, x7, x8| (x1, x2, x4, x6, x8)

# Call with mismatched types - args 1, 3, 5, and 7 should all be the same type 'a'
# but we're passing U64, Str, F64, Bool which are all different
result = multi_arg_fn((
	42, # x1: U64 (type 'a')
	"hello", # x2: Str (type 'b') - correct
	"world", # x3: Str (should be 'a' = U64) - MISMATCH  
	1.5, # x4: F64 (type 'c') - correct
	3.14, # x5: F64 (should be 'a' = U64) - MISMATCH
	[1, 2], # x6: List I64 (type 'd') - correct
	True, # x7: Bool (should be 'a' = U64) - MISMATCH
	"done",
))

# x8: Str (type 'e') - correct
~~~
# EXPECTED
UNUSED VARIABLE - lambda_multi_arg_mismatch.md:5:25:5:27
UNUSED VARIABLE - lambda_multi_arg_mismatch.md:5:33:5:35
UNUSED VARIABLE - lambda_multi_arg_mismatch.md:5:41:5:43
TYPE MISMATCH - lambda_multi_arg_mismatch.md:11:5:11:5
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "multi_arg_fn"))
    (type type_23)
  )
  (Stmt.assign
    (pattern (Patt.ident "multi_arg_fn"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "result"))
    (Expr.fn_call)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 70
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 -> #67)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 -> #66)
(var #40 -> #67)
(var #41 _)
(var #42 -> #55)
(var #43 -> #69)
(var #44 Num *)
(var #45 Str)
(var #46 Str)
(var #47 F64)
(var #48 F64)
(var #49 Num *)
(var #50 Num *)
(var #51 _)
(var #52 _)
(var #53 Str)
(var #54 -> #68)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 _)
(var #66 tuple)
(var #67 fn_pure)
(var #68 tuple)
(var #69 fn_pure)
~~~
# TYPES
~~~roc
x1 : _f
x2 : _f
x5 : _f
x4 : _f
x3 : _f
x8 : _f
x6 : _f
multi_arg_fn : _arg, _arg2, _arg3, _arg4, _arg5, _arg6, _arg7, _arg8 -> (_field, _field2, _field3, _field4, _field5)
x7 : _f
result : _f
~~~
