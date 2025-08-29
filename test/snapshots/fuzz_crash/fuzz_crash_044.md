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
app { f: "" platform [] }

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
NIL
# CANONICALIZE
~~~clojure
(Expr.block
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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
