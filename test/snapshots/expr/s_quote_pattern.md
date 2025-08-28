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
(match
  (scrutinee     (ellipsis)
))
~~~
# FORMATTED
~~~roc
match ...
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:8 to 2:8

**Parse Error**
at 3:13 to 3:13

**Parse Error**
at 4:4 to 4:4

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
