# META
~~~ini
description=Singleline formatting hosted
type=file
~~~
# SOURCE
~~~roc
hosted [a!, b!]

a! : Str => Str
b! : Str => Str
~~~
# TOKENS
~~~text
KwHosted OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent ~~~
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
hosted [a!, b!]

[a!, b!]
a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
NIL
# PROBLEMS
**Expected Exposes**
at 1:1 to 1:8

**Unsupported Node**
at 3:6 to 3:9

**Unsupported Node**
at 4:6 to 4:9

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.list_literal)
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
