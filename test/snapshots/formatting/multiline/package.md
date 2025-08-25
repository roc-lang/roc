# META
~~~ini
description=Multiline formatting package
type=file
~~~
# SOURCE
~~~roc
package
	[
		a!,
		b!,
	]
	{
		a: "a",
		b: "b",
	}

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
package [
	a!,
	b!,
] packages {a, ((
	"a",
	b,
): "b", <malformed>)}

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
at 9:2 to 9:2

**Expected Close Curly Brace**
at 1:1 to 11:1

**Parse Error**
at 11:4 to 11:4

**Parse Error**
at 11:10 to 11:10

**Parse Error**
at 12:4 to 12:4

**Parse Error**
at 12:10 to 12:10

**Unsupported Node**
at 11:4 to 11:4

**Unsupported Node**
at 11:10 to 11:10

**Unsupported Node**
at 12:4 to 12:4

**Unsupported Node**
at 12:10 to 12:10

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
