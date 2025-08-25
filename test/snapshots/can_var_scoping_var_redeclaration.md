# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

# Test var redeclaration (should produce shadowing warning)
redeclareTest = |_| {
	var x_ = 5
	var x_ = 10 # Redeclare var - should warn but proceed
	x_ = 15 # Reassign - should work without warning
	x_
}

result = redeclareTest({})
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpAssign OpBar Underscore OpBar OpenCurly KwVar LowerIdent OpAssign Int KwVar LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent CloseCurly LowerIdent OpAssign LowerIdent OpenRound OpenCurly CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "redeclareTest")
    (lambda
      (body
        (block
          (binop_equals
            (var_lc "x_")
            (num_literal_i32 5)
          )
          (binop_equals
            (var_lc "x_")
            (num_literal_i32 10)
          )
          (binop_equals
            (lc "x_")
            (num_literal_i32 15)
          )
          (binop_colon
            (lc "x_")
            (lc "x_")
          )
        )
      )
      (args
        (underscore)
      )
    )
  )
  (binop_equals
    (lc "result")
    (apply_lc
      (lc "redeclareTest")
      (record_literal)
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

redeclareTest = \_ -> {
	var x_ = 5
	var x_ = 10 # Redeclare var - should warn but proceed
	x_ = 15 # Reassign - should work without warning
	x_: x_
}

result = redeclareTest({  })
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 4:17 to 4:21

# CANONICALIZE
~~~clojure
(Expr.block
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
