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
KwMatch TripleDot OpenCurly OpenSquare SingleQuote CloseSquare OpThinArrow TripleDot OpenSquare SingleQuote Comma SingleQuote CloseSquare OpThinArrow TripleDot Underscore OpThinArrow TripleDot CloseCurly ~~~
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


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 16
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
~~~
# TYPES
~~~roc
~~~
