# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

# Regular function with var usage
processItems = |items| {
	var count_ = 0
	var total_ = 0

	# Reassign vars within same function - should work
	count_ = count_ + 1
	total_ = total_ + 10

	# Nested function - var reassignment should fail across function boundary
	nestedFunc = |_| {
		count_ = count_ + 5 # Should cause error - different function
		total_ = total_ * 2 # Should cause error - different function
		count_
	}

	result = nestedFunc({})
	total_ + result
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwVar LowerIdent OpAssign Int KwVar LowerIdent OpAssign Int LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent OpAssign LowerIdent OpStar Int LowerIdent CloseCurly LowerIdent OpAssign LowerIdent OpenRound OpenCurly CloseCurly CloseRound LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "processItems")
    (lambda
      (body
        (block
          (binop_equals
            (var_lc "count_")
            (num_literal_i32 0)
          )
          (binop_equals
            (var_lc "total_")
            (num_literal_i32 0)
          )
          (binop_equals
            (lc "count_")
            (binop_plus
              (lc "count_")
              (num_literal_i32 1)
            )
          )
          (binop_equals
            (lc "total_")
            (binop_plus
              (lc "total_")
              (num_literal_i32 10)
            )
          )
          (binop_equals
            (lc "nestedFunc")
            (lambda
              (body
                (block
                  (binop_equals
                    (lc "count_")
                    (binop_plus
                      (lc "count_")
                      (num_literal_i32 5)
                    )
                  )
                  (binop_equals
                    (lc "total_")
                    (binop_star
                      (lc "total_")
                      (num_literal_i32 2)
                    )
                  )
                  (binop_colon
                    (lc "count_")
                    (lc "count_")
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
              (lc "nestedFunc")
              (record_literal)
            )
          )
          (binop_plus
            (lc "total_")
            (lc "result")
          )
        )
      )
      (args
        (lc "items")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

processItems = |items| {
	var count_ = 0
	var total_ = 0
	count_ = count_ + 1
	total_ = total_ + 10
	nestedFunc = |_| {
		count_ = count_ + 5 # Should cause error - different function
		total_ = total_ * 2 # Should cause error - different function
		count_ : count_
	}
	result = nestedFunc({  })
	total_ + result
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "processItems")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
processItems : _a
~~~
