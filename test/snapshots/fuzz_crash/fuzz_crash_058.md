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
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
app { f: "" platform [] }

~~~
# EXPECTED
NIL
# PROBLEMS
**Expected String**
at 1:1 to 1:22

**Expected Close Curly Brace**
at 1:1 to 1:22

**Parse Error**
at 1:22 to 1:22

**Parse Error**
at 2:1 to 2:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
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
