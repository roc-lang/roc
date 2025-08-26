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
KwHosted OpenSquare LowerIdent OpBang Comma LowerIdent OpBang Comma CloseSquare LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent LowerIdent OpBang OpColon UpperIdent OpFatArrow UpperIdent ~~~
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
hosted [
	a!,
	b!,
]

[
	a!,
	b!,
]

a!: (Str => Str)
b!: (Str => Str)
~~~
# EXPECTED
NIL
# PROBLEMS
**Expected Exposes**
at 1:1 to 1:8

**Unsupported Node**
at 1:8 to 4:1

**Unsupported Node**
at 6:6 to 6:16

**Unsupported Node**
at 7:6 to 7:16

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
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
