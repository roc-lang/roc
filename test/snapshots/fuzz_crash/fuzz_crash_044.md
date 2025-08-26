# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{{0
}}

""
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpenCurly OpenCurly Int CloseCurly CloseCurly String ~~~
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
    (block
      (num_literal_i32 0)
    )
  )
  (str_literal_small "")
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
	{
		0
	}
}

""
~~~
# EXPECTED
NIL
# PROBLEMS
**Expected Open Curly Brace**
at 1:1 to 1:4

**Parse Error**
at 1:9 to 1:9

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_or)
  (Expr.block
    (Expr.malformed)
    (Expr.binop_not_equals)
  )
  (Expr.block
    (Expr.block
      (Expr.binop_star)
    )
  )
  (Expr.binop_not_equals)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
