# META
~~~ini
description=Multiline formatting app
type=file
~~~
# SOURCE
~~~roc
app [
	a1!,
	a2!,
] {
	pf: platform "../basic-cli/main.roc",
	a: "a",
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang Comma LowerIdent OpBang Comma CloseSquare OpenCurly LowerIdent OpColon KwPlatform String Comma LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(block
  (list_literal
    (not_lc "a1")
    (not_lc "a2")
  )
  (record_literal
    (binop_colon
      (lc "pf")
      (malformed malformed:expr_unexpected_token)
    )
    (str_literal_big "../basic-cli/main.roc")
    (binop_colon
      (lc "a")
      (str_literal_small "a")
    )
  )
)
~~~
# FORMATTED
~~~roc
app
{
	a1!,
	a2!,
}

[
	a1!,
	a2!,
]
{
	pf: platform,
	"../basic-cli/main.roc",
	a: "a",
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Expected Open Curly Brace**
at 1:1 to 1:5

**Parse Error**
at 5:6 to 5:6

**Unsupported Node**
at 1:5 to 4:1

**Unsupported Node**
at 5:6 to 5:6

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.record_literal
    (Expr.binop_colon
      (Expr.lookup "pf")
      (Expr.malformed)
    )
    (Expr.str_literal_big)
    (Expr.binop_colon
      (Expr.lookup "a")
      (Expr.str_literal_small)
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
