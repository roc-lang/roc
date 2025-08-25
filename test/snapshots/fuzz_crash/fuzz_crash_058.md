# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform"",r:"
}
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String Comma LowerIdent OpColon MalformedString CloseCurly ~~~
# PARSE
~~~clojure
(block
  (list_literal)
  (record_literal
    (binop_colon
      (lc "f")
      (malformed malformed:expr_unexpected_token)
    )
    (str_literal_small "")
    (binop_colon
      (lc "r")
      (malformed malformed:expr_unexpected_token)
    )
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
	f: platform,
	"",
	r: "
}
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
at 1:22 to 1:22

**Unsupported Node**
at 1:4 to 1:5

**Unsupported Node**
at 1:9 to 1:9

**Unsupported Node**
at 1:22 to 1:22

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.record_literal
    (Expr.binop_colon
      (Expr.lookup "f")
      (Expr.malformed)
    )
    (Expr.str_literal_small)
    (Expr.binop_colon
      (Expr.lookup "r")
      (Expr.malformed)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "{}")
~~~
# TYPES
~~~roc
~~~
