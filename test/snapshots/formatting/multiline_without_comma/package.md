# META
~~~ini
description=Multiline without comma formatting package
type=file
~~~
# SOURCE
~~~roc
package
	[
		a!,
		b!
	]
	{
		a: "a",
		b: "b"
	}

a! : Str => Str
b! : Str => Str
~~~
# TOKENS
~~~text
KwPackage OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String CloseCurly LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent ~~~
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
package [a, b] packages {a, ("a", b) : "b"}

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 11:6 to 11:9

**Unsupported Node**
at 12:6 to 12:9

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.not_lookup)
    (Expr.binop_thick_arrow)
  )
  (Expr.binop_colon
    (Expr.not_lookup)
    (Expr.binop_thick_arrow)
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
