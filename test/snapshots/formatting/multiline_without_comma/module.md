# META
~~~ini
description=Multiline without comma formatting module
type=file
~~~
# SOURCE
~~~roc
module [
	a,
	b
]

a = 'a'
b = 'a'
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign SingleQuote LowerIdent OpAssign SingleQuote ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "a")

    (lc "b")
))
~~~
# FORMATTED
~~~roc
module [a, b]

a = 'a'
b = 'a'
b = 'a'
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "a")
    (Expr.str_literal_small)
  )
  (Expr.binop_equals
    (Expr.lookup "b")
    (Expr.str_literal_small)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
a : Str
b : Str
~~~
