# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}|(0,)|||0
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpBar OpenRound Int Comma CloseRound OpOr OpBar Int ~~~
# PARSE
~~~clojure
(block
  (list_literal)
  (binop_pipe
    (block
      (binop_colon
        (lc "f")
        (malformed malformed:expr_unexpected_token)
      )
      (str_literal_small "")
    )
    (tuple_literal
      (num_literal_i32 0)
      (binop_or
        (malformed malformed:expr_unexpected_token)
        (malformed malformed:expr_unexpected_token)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app
{
	f,
	platform,
}

[]
{
	f : platform
	""
} | (0, ) || )
~~~
# EXPECTED
NIL
# PROBLEMS
**Expected Open Curly Brace**
at 1:1 to 1:4

**Parse Error**
at 1:9 to 1:9

**Parse Error**
at 1:24 to 1:24

**Parse Error**
at 1:29 to 1:29

**Parse Error**
at 1:29 to 1:29

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_or)
  (Expr.frac_literal_big)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
