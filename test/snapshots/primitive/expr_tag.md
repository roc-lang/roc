# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
foo = FortyTwo
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign UpperIdent ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "foo")
    (uc "FortyTwo")
  )
)
~~~
# FORMATTED
~~~roc
module [foo]

foo = FortyTwo
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
