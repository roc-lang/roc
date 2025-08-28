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
package [a, b] packages {a, (
	(
		"a",
		b,
	) : "b",
)}

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:37 to 1:37

**Expected Close Curly Brace**
at 1:1 to 3:1

# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_c")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
