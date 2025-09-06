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
(block
  (binop_equals
    (lc "x")
    (tuple_literal
      (str_literal_small "one")
      (str_literal_small "two")
      (str_literal_small "u")
      (str_literal_small "u)")
      (str_literal_small "u(")
      (str_literal_small "u()")
      (str_literal_small "u(K)")
      (str_literal_big "u(1F680)")
    )
  )
  (malformed)
)
~~~
# FORMATTED
~~~roc
module []

x = ("one", "two", "\u", "\u)", "\u(", "\u()", "\u(K)", "\u(1F680)")
# Test backslash before EOF
"\
~~~
# EXPECTED
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
INVALID ESCAPE SEQUENCE - :0:0:0:0
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - string.md:15:1:15:2
PARSE ERROR - string.md:15:2:15:3
PARSE ERROR - string.md:15:3:15:3
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
  (Stmt.assign
    (pattern (Patt.ident "x"))
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
; Total type variables: 16
(var #0 _)
(var #1 -> #14)
(var #2 Str)
(var #3 Str)
(var #4 Str)
(var #5 Str)
(var #6 Str)
(var #7 Str)
(var #8 Str)
(var #9 Str)
(var #10 -> #14)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 tuple)
(var #15 _)
~~~
# TYPES
~~~roc
x : (Str, Str, Str, Str, Str, Str, Str, Str)
~~~
