# META
~~~ini
description=Multiline formatting hosted
type=file
~~~
# SOURCE
~~~roc
hosted [
	a!,
	b!,
]

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
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "a")
    (type binop_thick_arrow)
  )
  (Stmt.type_anno
    (name "b")
    (type binop_thick_arrow)
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
