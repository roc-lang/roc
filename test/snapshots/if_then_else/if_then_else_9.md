# META
~~~ini
description=if_then_else (9)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	1
} else if 10 { # Comment after else open
	A
} else { # Comment after else open
	3
}
~~~
# TOKENS
~~~text
KwIf LowerIdent OpenCurly Int CloseCurly KwElse KwIf Int OpenCurly LineComment UpperIdent CloseCurly KwElse OpenCurly LineComment Int CloseCurly ~~~
# PARSE
~~~clojure
(if_else
  (condition     (lc "bool")
)
  (then     (block
      (num_literal_i32 1)
    )
)
  (else     (if_else
      (condition         (num_literal_i32 10)
)
      (then         (block
          (uc "A")
        )
)
      (else         (block
          (num_literal_i32 3)
        )
))
))
~~~
# FORMATTED
~~~roc
if bool
	{
		1
	}
else if 10
	{
		A
	}
else {
	3
}# Comment after else open
# Comment after else open
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
(expr :tag if_else :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
