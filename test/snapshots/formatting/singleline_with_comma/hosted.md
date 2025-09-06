# META
~~~ini
description=Singleline with comma formatting hosted
type=file
~~~
# SOURCE
~~~roc
hosted [a!, b!,]

a! : Str => Str
b! : Str => Str
~~~
# TOKENS
~~~text
KwHosted OpenSquare LowerIdent OpBang Comma LowerIdent OpBang Comma CloseSquare BlankLine LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent ~~~
# PARSE
~~~clojure
(hosted-header
  (exposes
    (not_lc "a")

    (not_lc "b")
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
hosted [
	a!,
	b!,
]

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - hosted.md:1:9:1:11
EXPOSED BUT NOT DEFINED - hosted.md:1:13:1:15
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "a"))
    (type type_6)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "b"))
    (type type_11)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 14
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
~~~
# TYPES
~~~roc
a : _c
b : _c
~~~
