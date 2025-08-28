# META
~~~ini
description=Basic variable scoping behavior
type=file
~~~
# SOURCE
~~~roc
module []

# Top-level variables
x = 5
y = 10

# Function that shadows outer variable
outerFunc = |_| {
    x = 20  # Should shadow top-level x
    innerResult = {
        # Block scope
        z = x + y  # x should resolve to 20, y to 10
        z + 1
    }
    innerResult
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign OpenCurly LowerIdent OpAssign LowerIdent OpPlus LowerIdent LowerIdent OpPlus Int CloseCurly LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (num_literal_i32 5)
  )
  (binop_equals
    (lc "y")
    (num_literal_i32 10)
  )
  (binop_equals
    (lc "outerFunc")
    (lambda
      (body
        (block
          (binop_equals
            (lc "x")
            (num_literal_i32 20)
          )
          (binop_equals
            (lc "innerResult")
            (block
              (binop_equals
                (lc "z")
                (binop_plus
                  (lc "x")
                  (lc "y")
                )
              )
              (binop_plus
                (lc "z")
                (num_literal_i32 1)
              )
            )
          )
          (binop_colon
            (lc "innerResult")
            (lc "innerResult")
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
module []

x = 5
y = 10
outerFunc = \_ -> {
	x = 20 # Should shadow top-level x
	innerResult = {
				# Block scope
z = x + y # x should resolve to 20, y to 10
		z + 1
	}
	innerResult : innerResult
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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
