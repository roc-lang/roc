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
KwModule OpenSquare CloseSquare BlankLine LineComment LowerIdent OpAssign OpBar Underscore OpBar OpenCurly KwVar LowerIdent OpAssign Int KwVar LowerIdent OpAssign Int LineComment LowerIdent OpAssign Int LineComment LowerIdent CloseCurly BlankLine LowerIdent OpAssign LowerIdent OpenRound OpenCurly CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []


# Test var redeclaration (should produce shadowing warning)
redeclareTest = |_| {
	var x_ = 5
	var x_ = 10
	# Redeclare var - should warn but proceed
	x_ = 15
	# Reassign - should work without warning
	x_ : x_
}


result = redeclareTest({})
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **x_** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x_` to suppress this warning.
The unused variable is declared here:

**can_var_scoping_var_redeclaration.md:8:2:8:4:**
```roc
	x_
```
	^^


# CANONICALIZE
~~~clojure
(Expr.block
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
