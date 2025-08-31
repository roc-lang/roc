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
		count_ = count_ + 5
		total_ = total_ * 2
		count_ : count_
	}

	result = nestedFunc({})
	total_ + result
}

# Regular function with var usage
# Reassign vars within same function - should work
# Nested function - var reassignment should fail across function boundary
# Should cause error - different function
# Should cause error - different function
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
