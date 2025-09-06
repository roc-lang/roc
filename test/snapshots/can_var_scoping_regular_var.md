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
KwModule OpenSquare CloseSquare BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwVar LowerIdent OpAssign Int KwVar LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent OpAssign LowerIdent OpPlus Int BlankLine LineComment LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpPlus Int LineComment LowerIdent OpAssign LowerIdent OpStar Int LineComment LowerIdent CloseCurly BlankLine LowerIdent OpAssign LowerIdent OpenRound OpenCurly CloseCurly CloseRound LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
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

# Regular function with var usage
processItems = |items| {
	var count_ = 0
	var total_ = 0
	# Reassign vars within same function - should work
	count_ = count_ + 1
	total_ = total_ + 10
	# Nested function - var reassignment should fail across function boundary
	nestedFunc = |_| {
		count_ = count_ + 5
		# Should cause error - different function
		total_ = total_ * 2
		# Should cause error - different function
		count_ : count_
	}

	result = nestedFunc({})
	total_ + result
}
~~~
# EXPECTED
VAR REASSIGNMENT ERROR - :0:0:0:0
VAR REASSIGNMENT ERROR - :0:0:0:0
UNUSED VARIABLE - can_var_scoping_regular_var.md:4:17:4:22
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "processItems"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 54
(var #0 _)
(var #1 -> #53)
(var #2 _)
(var #3 _)
(var #4 Num *)
(var #5 _)
(var #6 _)
(var #7 Num *)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 Num *)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 Num *)
(var #17 _)
(var #18 _)
(var #19 -> #50)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 Num *)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 Num *)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 -> #50)
(var #35 _)
(var #36 -> #39)
(var #37 -> #52)
(var #38 -> #51)
(var #39 _)
(var #40 _)
(var #41 -> #42)
(var #42 -> #43)
(var #43 _)
(var #44 _)
(var #45 -> #53)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 fn_pure)
(var #51 {})
(var #52 fn_pure)
(var #53 fn_pure)
~~~
# TYPES
~~~roc
total_ : _a
count_ : _a
result : _a
nestedFunc : _arg -> _ret
processItems : _arg -> _ret
items : _a
~~~
