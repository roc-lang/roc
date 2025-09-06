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
KwModule OpenSquare CloseSquare BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign LowerIdent LineComment KwVar LowerIdent OpAssign LowerIdent OpStar Int LineComment BlankLine LowerIdent OpAssign LowerIdent OpPlus LowerIdent LineComment LowerIdent OpPlus LowerIdent LineComment CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

# Function showing var vs regular identifier independence
testFunc = |input| {
	sum = input
	# Regular identifier
	var sum_ = input * 2
	# Var with underscore - should not conflict

	sum_ = sum_ + sum
	# Reassign var - should work
	sum + sum_
}

# Both should be accessible
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "testFunc"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
