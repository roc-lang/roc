# META
~~~ini
description=Lambda parameters with unused variable checking
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

# Lambda with unused parameter - should warn
add : U64 -> U64
add = |unused| 42

# Lambda with underscore parameter that is used - should warn
multiply : U64 -> U64
multiply = |_factor| _factor * 2

# Lambda with unused underscore parameter - should be fine
process : U64 -> U64
process = |_input| 100

# Lambda with used parameter - should be fine
double : U64 -> U64
double = |value| value * 2

main! = |_| {
    result1 = add(5)
    result2 = multiply(3)
    result3 = process(7)
    result4 = double(4)
    result1 + result2 + result3 + result4
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "add")
    (binop_thin_arrow
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "add")
    (lambda
      (body
        (num_literal_i32 42)
      )
      (args
        (lc "unused")
      )
    )
  )
  (binop_colon
    (lc "multiply")
    (binop_thin_arrow
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "multiply")
    (lambda
      (body
        (binop_star
          (lc "_factor")
          (num_literal_i32 2)
        )
      )
      (args
        (lc "_factor")
      )
    )
  )
  (binop_colon
    (lc "process")
    (binop_thin_arrow
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "process")
    (lambda
      (body
        (num_literal_i32 100)
      )
      (args
        (lc "_input")
      )
    )
  )
  (binop_colon
    (lc "double")
    (binop_thin_arrow
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "double")
    (lambda
      (body
        (binop_star
          (lc "value")
          (num_literal_i32 2)
        )
      )
      (args
        (lc "value")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (block
          (binop_equals
            (lc "result1")
            (apply_lc
              (lc "add")
              (num_literal_i32 5)
            )
          )
          (binop_equals
            (lc "result2")
            (apply_lc
              (lc "multiply")
              (num_literal_i32 3)
            )
          )
          (binop_equals
            (lc "result3")
            (apply_lc
              (lc "process")
              (num_literal_i32 7)
            )
          )
          (binop_equals
            (lc "result4")
            (apply_lc
              (lc "double")
              (num_literal_i32 4)
            )
          )
          (binop_plus
            (binop_plus
              (binop_plus
                (lc "result1")
                (lc "result2")
              )
              (lc "result3")
            )
            (lc "result4")
          )
        )
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app
{
	pf: "../basic-cli/main.roc" platform [
		main,
	],
}

add : U64 -> U64
add = \unused -> 42

# Lambda with underscore parameter that is used - should warn
multiply : U64 -> U64
multiply = \_factor -> _factor * 2

# Lambda with unused underscore parameter - should be fine
process : U64 -> U64
process = \_input -> 100

# Lambda with used parameter - should be fine
double : U64 -> U64
double = \value -> value * 2

main! = \_ -> {
	result1 = add(5)
	result2 = multiply(3)
	result3 = process(7)
	result4 = double(4)
	((result1 + result2) + result3) + result4
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
