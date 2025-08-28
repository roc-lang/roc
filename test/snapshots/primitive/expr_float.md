# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
foo = 12.34
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign Float ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "foo")
    (frac_literal_small 12.34)
  )
)
~~~
# FORMATTED
~~~roc
module [foo]

foo = 12.34
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
