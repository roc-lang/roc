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
(package-header
  (exposes
    (lc "a")

    (lc "b")
)
  (packages
    (lc "a")

    (tuple_literal
      (binop_colon
        (tuple_literal
          (str_literal_small "a")
          (lc "b")
        )
        (str_literal_small "b")
      )
      (malformed malformed:expr_unexpected_token)
    )
))
~~~
# FORMATTED
~~~roc
package [
	a,
	b,
] packages {a, (
	("a", b) : "b",
)}

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 9:2 to 11:1

**Expected Close Curly Brace**
at 1:1 to 11:1

**Unsupported Node**
at 11:6 to 11:9

**Unsupported Node**
at 12:6 to 12:9

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.not_lookup)
    (Expr.binop_thick_arrow
      (Expr.malformed)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_colon
    (Expr.not_lookup)
    (Expr.binop_thick_arrow
      (Expr.malformed)
      (Expr.apply_tag)
    )
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
