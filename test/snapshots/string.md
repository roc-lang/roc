# META
~~~ini
description=two strings
type=file
~~~
# SOURCE
~~~roc
module []

x = (
	"one",
	"two",
	"\u",
	"\u)",
	"\u(",
	"\u()",
	"\u(K)",
	"\u(1F680)",
)

# Test backslash before EOF
"\
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpAssign OpenRound String Comma String Comma String Comma String Comma String Comma String Comma String Comma String Comma CloseRound BlankLine LineComment MalformedString ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

x = ("one", "two", "\u", "\u)", "\u(", "\u()", "\u(K)", "\u(1F680)")
"\
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **"\** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**string.md:15:1:15:3:**
```roc
"\
```
^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "x")
    (Expr.tuple_literal
      (Expr.str_literal_small)
      (Expr.str_literal_small)
      (Expr.str_literal_small)
      (Expr.str_literal_small)
      (Expr.str_literal_small)
      (Expr.str_literal_small)
      (Expr.str_literal_small)
      (Expr.str_literal_big)
    )
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
x : _a
~~~
