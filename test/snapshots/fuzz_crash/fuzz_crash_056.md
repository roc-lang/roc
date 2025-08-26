# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform"",r:"",o:""}
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String Comma LowerIdent OpColon String Comma LowerIdent OpColon String CloseCurly ~~~
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
      (str_literal_small "")
    )
    (binop_colon
      (lc "o")
      (str_literal_small "")
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

[]{ f : platform, "", r : "", o : "" }
~~~
# EXPECTED
NIL
# PROBLEMS
**Expected Open Curly Brace**
at 1:1 to 1:4

**Parse Error**
at 1:9 to 1:9

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_or)
  (Expr.binop_double_slash)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
