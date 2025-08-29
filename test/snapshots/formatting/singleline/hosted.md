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
(block
  (list_literal
    (not_lc "a")
    (not_lc "b")
  )
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
