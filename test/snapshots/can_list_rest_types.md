# META
~~~ini
description=List rest patterns should have correct list types matching element types
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [first, .. as restNums] => restNums
    [] => []
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare CloseSquare OpFatArrow OpenSquare CloseSquare CloseCurly ~~~
# PARSE
~~~clojure
(match <10 branches>)
~~~
# FORMATTED
~~~roc
when numbers is {
	[(first, <unary_double_dot>)]
	restNums
	<malformed>
	<malformed>
	restNums
	[]
	<malformed>
	[]
} -> <malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:15

**Parse Error**
at 2:16 to 2:16

**Parse Error**
at 2:5 to 2:19

**Parse Error**
at 2:27 to 2:27

**Parse Error**
at 2:29 to 2:29

**Parse Error**
at 3:8 to 3:8

**Parse Error**
at 1:1 to 4:2

**Parse Error**
at 4:2 to 4:2

**Unsupported Node**
at 1:15 to 4:1

**Unsupported Node**
at 4:2 to 4:2

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
