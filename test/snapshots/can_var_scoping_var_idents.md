# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

# Function showing var vs regular identifier independence
testFunc = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign LowerIdent KwVar LowerIdent OpAssign LowerIdent OpStar Int LowerIdent OpAssign LowerIdent OpPlus LowerIdent LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "testFunc")
    (lambda
      (body
        (block
          (binop_equals
            (lc "sum")
            (lc "input")
          )
          (binop_equals
            (var_lc "sum_")
            (binop_star
              (lc "input")
              (num_literal_i32 2)
            )
          )
          (binop_equals
            (lc "sum_")
            (binop_plus
              (lc "sum_")
              (lc "sum")
            )
          )
          (binop_plus
            (lc "sum")
            (lc "sum_")
          )
        )
      )
      (args
        (lc "input")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

testFunc = \input -> {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
