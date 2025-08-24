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

**Pattern in Expression Context**
at 6:6 to 6:9

**Unsupported Node**
at 6:10 to 6:10

**Pattern in Expression Context**
at 6:13 to 6:16

**Unsupported Node**
at 7:4 to 7:4

**Pattern in Expression Context**
at 7:6 to 7:9

**Unsupported Node**
at 7:10 to 7:10

**Pattern in Expression Context**
at 7:13 to 7:16

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "b")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "a")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "b")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
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
