# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import fS
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport LowerIdent UpperIdent ~~~
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
  (import
    (lc "f")
  )
  (uc "S")
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
	f: platform
	""
}import f
S
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

**Unsupported Node**
at 1:20 to 1:28

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
  (Expr.malformed)
  (Expr.apply_tag)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "[]_others")
~~~
# TYPES
~~~roc
~~~
