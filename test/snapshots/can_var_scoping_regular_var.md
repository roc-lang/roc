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
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **count_** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_count_` to suppress this warning.
The unused variable is declared here:

**can_var_scoping_regular_var.md:16:3:16:9:**
```roc
		count_
```
		^^^^^^


**UNUSED VARIABLE**
Variable **items** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_items` to suppress this warning.
The unused variable is declared here:

**can_var_scoping_regular_var.md:4:17:4:22:**
```roc
processItems = |items| {
```
                ^^^^^


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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
