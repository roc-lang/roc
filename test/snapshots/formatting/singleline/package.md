# META
~~~ini
description=Singleline formatting package
type=file
~~~
# SOURCE
~~~roc
package [a!, b!] { a: "a", b: "b" }

a! : Str => Str
b! : Str => Str
~~~
# TOKENS
~~~text
KwPackage OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String CloseCurly LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent ~~~
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
package [
	a!, b!
] packages {a, ("a", b): "b"}

a<malformed>!Str
<malformed>
Str
b<malformed>!Str
<malformed>
Str
~~~
# EXPECTED
NIL
# PROBLEMS
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

**Unsupported Node**
at 3:10 to 3:10

**Unsupported Node**
at 4:4 to 4:4

**Unsupported Node**
at 4:10 to 4:10

# CANONICALIZE
~~~clojure
(Expr.block
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
