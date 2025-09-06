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
KwPackage OpenSquare LowerIdent OpBang Comma LowerIdent OpBang Comma CloseSquare OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String Comma CloseCurly BlankLine LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent ~~~
# PARSE
~~~clojure
(package-header
  (exposes
    (not_lc "a")

    (not_lc "b")
)
  (packages
    (lc "a")

    (binop_colon
      (tuple_literal
        (str_literal_small "a")
        (lc "b")
      )
      (str_literal_small "b")
    )
))
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
	a!,
	b!,
] packages {a, ("a", b) : "b"}

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package.md:1:10:1:12
EXPOSED BUT NOT DEFINED - package.md:1:14:1:16
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "a"))
    (type type_12)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "b"))
    (type type_17)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 20
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
~~~
# TYPES
~~~roc
a : _c
b : _c
~~~
