# META
~~~ini
description=Singleline formatting module
type=file
~~~
# SOURCE
~~~roc
module [a, b]

a = 'a'
b = 'a'
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign SingleQuote LowerIdent OpAssign SingleQuote ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "a")
    (str_literal_small "")
  )
  (binop_equals
    (lc "b")
    (str_literal_small "")
  )
)
~~~
# FORMATTED
~~~roc
module [
	a, b
]


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
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
