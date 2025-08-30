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
KwModule OpenSquare CloseSquare LowerIdent OpAssign OpenRound String Comma String Comma String Comma String Comma String Comma String Comma String Comma String Comma CloseRound MalformedString ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
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
"\
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 12:1 to 15:1

**Parse Error**
at 15:1 to 15:1

**Parse Error**
at 15:1 to 15:3

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
      (Expr.malformed)
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
