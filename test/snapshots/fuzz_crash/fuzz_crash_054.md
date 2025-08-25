# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import S exposing[c as
f]
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport UpperIdent KwExposing OpenSquare LowerIdent KwAs LowerIdent CloseSquare ~~~
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
    (uc "S")
    (lc "c")
  )
  (malformed malformed:expr_unexpected_token)
  (lc "f")
  (malformed malformed:expr_unexpected_token)
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
}import S exposing [c]
as
f
~~~
# EXPECTED
NIL
# PROBLEMS
**Expected Open Curly Brace**
at 1:1 to 1:4

**Parse Error**
at 1:9 to 1:9

**Parse Error**
at 1:40 to 1:40

**Parse Error**
at 2:2 to 2:2

**Unsupported Node**
at 1:4 to 1:5

**Unsupported Node**
at 1:9 to 1:9

**Unsupported Node**
at 1:20 to 1:39

**Unsupported Node**
at 1:40 to 1:40

**Unsupported Node**
at 2:2 to 2:2

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
  (Expr.malformed)
  (Expr.lookup "f")
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
