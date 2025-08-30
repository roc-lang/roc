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
(module-header)
~~~
# FORMATTED
~~~roc
module []

testFunc = |input| {
	sum = input
	var sum_ = input * 2
	sum_ = sum_ + sum
	sum + sum_
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
    (Expr.lookup "testFunc")
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
testFunc : _a
~~~
