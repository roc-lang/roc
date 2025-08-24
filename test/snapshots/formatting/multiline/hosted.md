# META
~~~ini
description=Multiline formatting hosted
type=file
~~~
# SOURCE
~~~roc
hosted [
	a!,
	b!,
]

a! : Str => Str
b! : Str => Str
~~~
# TOKENS
~~~text
KwHosted OpenSquare LowerIdent OpBang Comma LowerIdent OpBang Comma CloseSquare LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent ~~~
# PARSE
~~~clojure
(block
  (list_literal
    (lc "a")
  )
  (unary_not <unary>)
  (lc "b")
  (unary_not <unary>)
  (malformed malformed:expr_unexpected_token)
  (lc "a")
  (unary_not <unary>)
  (uc "Str")
  (malformed malformed:expr_unexpected_token)
  (uc "Str")
  (lc "b")
  (unary_not <unary>)
  (uc "Str")
  (malformed malformed:expr_unexpected_token)
  (uc "Str")
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - hosted.md:2:2:2:4
EXPOSED BUT NOT DEFINED - hosted.md:3:2:3:4
# PROBLEMS
**Expected Exposes**
at 1:1 to 1:8

**Parse Error**
at 1:8 to 2:3

**Parse Error**
at 2:4 to 2:4

**Parse Error**
at 3:4 to 3:4

**Parse Error**
at 4:1 to 4:1

**Parse Error**
at 6:4 to 6:4

**Parse Error**
at 6:10 to 6:10

**Parse Error**
at 7:4 to 7:4

**Parse Error**
at 7:10 to 7:10

**Unsupported Node**
at 1:8 to 2:4

**Unsupported Node**
at 2:4 to 2:4

**Unsupported Node**
at 3:4 to 3:4

**Unsupported Node**
at 4:1 to 4:1

**Unsupported Node**
at 6:4 to 6:4

**Unsupported Node**
at 6:10 to 6:10

**Unsupported Node**
at 7:4 to 7:4

**Unsupported Node**
at 7:10 to 7:10

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.unary_not)
  (Expr.lookup "b")
  (Expr.unary_not)
  (Expr.malformed)
  (Expr.lookup "a")
  (Expr.unary_not)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.lookup "b")
  (Expr.unary_not)
  (Expr.apply_tag)
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
