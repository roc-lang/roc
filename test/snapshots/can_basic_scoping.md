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


# Top-level variables
x = 5
y = 10
# Function that shadows outer variable
outerFunc = |_| {
	x = 20
	# Should shadow top-level x
	innerResult = {
		# Block scope
		z = x + y
		# x should resolve to 20, y to 10
		z + 1
	}

	innerResult : innerResult
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **innerResult** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_innerResult` to suppress this warning.
The unused variable is declared here:

**can_basic_scoping.md:15:5:15:16:**
```roc
    innerResult
```
    ^^^^^^^^^^^


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
