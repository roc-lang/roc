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
	f: platform
	""
}import B.G
if 0 {  } else ||0
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

**Unsupported Node**
at 1:4 to 1:5

**Unsupported Node**
at 1:9 to 1:9

**Unsupported Node**
at 1:20 to 2:2

**Unsupported Node**
at 2:13 to 2:13

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
  (Expr.if_else)
  (Expr.num_literal_i32 0)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Num(_size)")
~~~
# TYPES
~~~roc
~~~
