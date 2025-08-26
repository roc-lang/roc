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
	f : platform
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

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_or)
  (Expr.block
    (Expr.malformed)
    (Expr.binop_not_equals)
  )
  (Expr.binop_plus)
  (Expr.malformed)
  (Expr.str_literal_big)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
