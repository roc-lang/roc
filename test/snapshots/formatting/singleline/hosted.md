# META
~~~ini
description=Singleline formatting hosted
type=file
~~~
# SOURCE
~~~roc
hosted [a!, b!]

a! : Str => Str
b! : Str => Str
~~~
# TOKENS
~~~text
KwHosted OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent ~~~
# PARSE
~~~clojure
(block
  (list_literal
    (lc "a")
  )
  (unary_not <unary>)
  (lc "b")
  (unary_not <unary>)
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
EXPOSED BUT NOT DEFINED - hosted.md:1:9:1:11
EXPOSED BUT NOT DEFINED - hosted.md:1:13:1:15
# PROBLEMS
**Expected Exposes**
at 1:1 to 1:8

**Parse Error**
at 1:8 to 1:10

**Parse Error**
at 1:11 to 1:11

**Parse Error**
at 1:15 to 1:15

**Parse Error**
at 3:4 to 3:4

**Parse Error**
at 3:10 to 3:10

**Parse Error**
at 4:4 to 4:4

**Parse Error**
at 4:10 to 4:10

**Unsupported Node**
at 1:8 to 1:11

**Unsupported Node**
at 1:11 to 1:11

**Unsupported Node**
at 1:15 to 1:15

**Unsupported Node**
at 3:4 to 3:4

**Unsupported Node**
at 3:10 to 3:10

**Unsupported Node**
at 4:4 to 4:4

**Unsupported Node**
at 4:10 to 4:10

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.unary_not)
  (Expr.lookup "b")
  (Expr.unary_not)
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
