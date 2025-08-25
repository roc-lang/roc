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
app {  }

[]{
	f: <malformed>
	""
}
{
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

**Unsupported Node**
at 1:4 to 1:5

**Unsupported Node**
at 1:9 to 1:9

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.block
    (Expr.binop_colon
      (Expr.lookup "f")
      (Expr.malformed)
    )
    (Expr.str_literal_small)
  )
  (Expr.block
    (Expr.block
      (Expr.num_literal_i32 0)
    )
  )
  (Expr.str_literal_small)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Str")
~~~
# TYPES
~~~roc
~~~
