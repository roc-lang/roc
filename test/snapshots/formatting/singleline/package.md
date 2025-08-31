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
KwPackage OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String CloseCurly BlankLine LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent ~~~
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
~~~
# FORMATTED
~~~roc
package [a!, b!] packages {a, ("a", b) : "b"}

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
