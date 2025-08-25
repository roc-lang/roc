# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
name = "luc"
foo = "hello ${name}"
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign String LowerIdent OpAssign String ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "name")
    (str_literal_small "luc")
  )
  (binop_equals
    (lc "foo")
    (str_literal_big "hello ${name}")
  )
)
~~~
# FORMATTED
~~~roc
module [
	foo,
]

name = "luc"
foo = "hello ${name}"
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
