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
)
  (branch1     (binop_thick_arrow
      (list_literal
        (str_literal_small "#")
      )
      (ellipsis)
    )
)
  (branch2     (binop_thick_arrow
      (list_literal
        (str_literal_small "a")
        (str_literal_small "b")
      )
      (ellipsis)
    )
)
  (branch3     (binop_thick_arrow
      (underscore)
      (ellipsis)
    )
))
~~~
# FORMATTED
~~~roc
match ...
	['#'] => ...
	['a', 'b'] => ...
	_ => ...
}] => ...
	['a', 'b'] => ...
	_ => ...
}, 'b'] => ...
	_ => ...
}] => ...
	_ => ...
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:8 to 2:10

**Unsupported Node**
at 3:2 to 3:12

**Unsupported Node**
at 4:4 to 4:6

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
