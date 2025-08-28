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
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
