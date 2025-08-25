# META
~~~ini
description=Comprehensive test for match branch scoping with variable isolation
type=expr
~~~
# SOURCE
~~~roc
match result {
    Ok(value) => value + 1
    Err(value) => value - 1
    Ok(different) => different * 2
    Err(different) => different / 2
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpPlus Int UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpBinaryMinus Int UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpStar Int UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpSlash Int CloseCurly ~~~
# PARSE
~~~clojure
(match <28 branches>)
~~~
# FORMATTED
~~~roc
when result is {
	Ok(value)
	<malformed>
	value + 1
	Err(value)
	<malformed>
	value - 1
	Ok(different)
	<malformed>
	different * 2
	Err(different)
	<malformed>
	different / 2
} -> <malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:14

**Parse Error**
at 2:15 to 2:15

**Parse Error**
at 3:16 to 3:16

**Parse Error**
at 4:19 to 4:19

**Parse Error**
at 5:20 to 5:20

**Parse Error**
at 1:1 to 6:2

**Parse Error**
at 6:2 to 6:2

**Unsupported Node**
at 1:14 to 6:1

**Unsupported Node**
at 6:2 to 6:2

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
