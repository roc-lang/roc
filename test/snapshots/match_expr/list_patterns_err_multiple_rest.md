# META
~~~ini
description=Match expression with more than one rest pattern not permitted, should error
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [.., middle, ..] => ... # error, multiple rest patterns not allowed
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare DoubleDot Comma LowerIdent Comma DoubleDot CloseSquare OpFatArrow TripleDot LineComment CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "numbers")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (unary_double_dot <unary>)
        (lc "middle")
        (unary_double_dot <unary>)
      )
      (ellipsis)
    )
))
~~~
# FORMATTED
~~~roc
match numbers
	[.., middle, ..] => ...
# error, multiple rest patterns not allowed
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**list_patterns_err_multiple_rest.md:2:5:2:28:**
```roc
    [.., middle, ..] => ... # error, multiple rest patterns not allowed
```
    ^^^^^^^^^^^^^^^^^^^^^^^


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
