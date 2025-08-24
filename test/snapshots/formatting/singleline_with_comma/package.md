# META
~~~ini
description=Singleline with comma formatting package
type=file
~~~
# SOURCE
~~~roc
package [a!, b!,] { a: "a", b: "b", }

a! : Str => Str
b! : Str => Str
~~~
# TOKENS
~~~text
KwPackage OpenSquare LowerIdent OpBang Comma LowerIdent OpBang Comma CloseSquare OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String Comma CloseCurly LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent ~~~
# PARSE
~~~clojure
(block
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
EXPOSED BUT NOT DEFINED - package.md:1:10:1:12
EXPOSED BUT NOT DEFINED - package.md:1:14:1:16
# PROBLEMS
**Parse Error**
at 1:37 to 1:37

**Expected Close Curly Brace**
at 1:1 to 3:1

**Parse Error**
at 3:4 to 3:4

**Parse Error**
at 3:10 to 3:10

**Parse Error**
at 4:4 to 4:4

**Parse Error**
at 4:10 to 4:10

**Unsupported Node**
at 3:4 to 3:4

**Pattern in Expression Context**
at 3:6 to 3:9

**Unsupported Node**
at 3:10 to 3:10

**Pattern in Expression Context**
at 3:13 to 3:16

**Unsupported Node**
at 4:4 to 4:4

**Pattern in Expression Context**
at 4:6 to 4:9

**Unsupported Node**
at 4:10 to 4:10

**Pattern in Expression Context**
at 4:13 to 4:16

# CANONICALIZE
~~~clojure
(Expr.block
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
