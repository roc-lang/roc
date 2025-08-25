# META
~~~ini
description=Single quote literal's in patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
	['#'] => ...
	['a', 'b'] => ...
	_ => ...
}
~~~
# TOKENS
~~~text
KwMatch TripleDot OpenCurly OpenSquare SingleQuote CloseSquare OpFatArrow TripleDot OpenSquare SingleQuote Comma SingleQuote CloseSquare OpFatArrow TripleDot Underscore OpFatArrow TripleDot CloseCurly ~~~
# PARSE
~~~clojure
(match <11 branches>)
~~~
# FORMATTED
~~~roc
when ... is {
	['#'] => ...
	['a', 'b'] => ...
	_ => ...
}]
	<malformed>
	...
	[('a', 'b'] => ...
	_ => ...
}, 'b'] => ...
	_ => ...
})]
	<malformed>
	...
	_
	<malformed>
	...
} -> <malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:11

**Parse Error**
at 2:8 to 2:8

**Parse Error**
at 3:13 to 3:13

**Parse Error**
at 4:4 to 4:4

**Parse Error**
at 1:1 to 5:2

**Parse Error**
at 5:2 to 5:2

**Unsupported Node**
at 1:7 to 1:10

**Unsupported Node**
at 1:11 to 5:1

**Unsupported Node**
at 5:2 to 5:2

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
