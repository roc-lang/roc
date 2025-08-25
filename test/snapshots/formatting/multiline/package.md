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
  (binop_colon
    (not_lc "a")
    (binop_thick_arrow
      (uc "Str")
      (uc "Str")
    )
  )
  (binop_colon
    (not_lc "b")
    (binop_thick_arrow
      (uc "Str")
      (uc "Str")
    )
  )
)
~~~
# FORMATTED
~~~roc
package [
	a,
	b,
] packages {a, (
	(
		"a",
		b,
	): "b"
)}

a!: (Str => Str)
b!: (Str => Str)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 9:2 to 9:2

**Expected Close Curly Brace**
at 1:1 to 11:1

**Unsupported Node**
at 11:6 to 11:16

**Unsupported Node**
at 12:6 to 12:16

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.not_lookup)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.not_lookup)
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
