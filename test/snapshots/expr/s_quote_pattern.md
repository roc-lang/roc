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
        (malformed)
      )
      (block
        (ellipsis)
        (binop_thick_arrow
          (list_literal
            (str_literal_small "a")
            (str_literal_small "b")
          )
          (ellipsis)
        )
      )
    )
)
  (branch2     (binop_thick_arrow
      (underscore)
      (ellipsis)
    )
))
~~~
# FORMATTED
~~~roc
match ...
	['#'] => 
		...
		['a', 'b'] => ...
	_ => ...
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN PATTERN**
The token **'#'** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**s_quote_pattern.md:2:3:2:6:**
```roc
	['#'] => ...
```
	 ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**s_quote_pattern.md:2:2:3:19:**
```roc
	['#'] => ...
	['a', 'b'] => ...
```


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
