# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{
o:0)
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpenCurly LowerIdent OpColon Int CloseRound ~~~
# PARSE
~~~clojure
(block
  (list_literal)
  (block
    (binop_colon
      (lc "f")
      (malformed malformed:expr_unexpected_token)
    )
    (str_literal_small "")
  )
  (block
    (binop_colon
      (lc "o")
      (num_literal_i32 0)
    )
    (malformed malformed:expr_unexpected_token)
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

[]{
	f : platform
	""
}{
	o : 0
	
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Expected Open Curly Brace**
at 1:1 to 1:4

**Parse Error**
at 1:9 to 1:9

**Parse Error**
at 2:4 to 2:4

**Parse Error**
at 1:20 to 2:5

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_or)
  (Expr.block
    (Expr.malformed)
    (Expr.binop_not_equals)
  )
  (Expr.block
    (Expr.malformed)
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
