# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import	B	as
G	if 0{}else||0
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport UpperIdent KwAs UpperIdent KwIf Int OpenCurly CloseCurly KwElse OpOr Int ~~~
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
    (uc "B")
    (uc "G")
  )
  (if_else <6 branches>)
  (num_literal_i32 0)
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
}import B.G
if 0 {  } else ||
0
~~~
# EXPECTED
NIL
# PROBLEMS
**Expected Open Curly Brace**
at 1:1 to 1:4

**Parse Error**
at 1:9 to 1:9

**Parse Error**
at 2:3 to 2:7

**Parse Error**
at 2:13 to 2:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_or)
  (Expr.block
    (Expr.malformed)
    (Expr.binop_not_equals)
  )
  (Expr.binop_plus)
  (Expr.match)
  (Expr.binop_star)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
