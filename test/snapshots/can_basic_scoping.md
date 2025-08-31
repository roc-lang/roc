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
KwModule OpenSquare CloseSquare BlankLine LineComment LowerIdent OpAssign Int LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign Int LineComment LowerIdent OpAssign OpenCurly LineComment LowerIdent OpAssign LowerIdent OpPlus LowerIdent LineComment LowerIdent OpPlus Int CloseCurly LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

x = 5
y = 10
outerFunc = |_| {
	x = 20
	innerResult = {
		z = x + y
		z + 1
	}

	innerResult : innerResult
}

# Top-level variables
# Function that shadows outer variable
# Should shadow top-level x
# Block scope
# x should resolve to 20, y to 10
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "x")
    (Expr.num_literal_i32 5)
  )
  (Expr.binop_equals
    (Expr.lookup "y")
    (Expr.num_literal_i32 10)
  )
  (Expr.binop_equals
    (Expr.lookup "outerFunc")
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
x : Num(_size)
y : Num(_size)
outerFunc : _a
~~~
