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
NO CHANGE
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package.md:3:3:3:5
EXPOSED BUT NOT DEFINED - package.md:4:3:4:5
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

**Pattern in Expression Context**
at 11:6 to 11:9

**Unsupported Node**
at 11:10 to 11:10

**Pattern in Expression Context**
at 11:13 to 11:16

**Unsupported Node**
at 12:4 to 12:4

**Pattern in Expression Context**
at 12:6 to 12:9

**Unsupported Node**
at 12:10 to 12:10

**Pattern in Expression Context**
at 12:13 to 12:16

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
