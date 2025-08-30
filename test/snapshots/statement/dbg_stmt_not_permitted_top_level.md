# META
~~~ini
description=Debug expression not permitted at the top level
type=file
~~~
# SOURCE
~~~roc
module [foo]

# not permitted
dbg "foo"

foo = ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare KwDbg String LowerIdent OpAssign TripleDot ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "foo")
))
~~~
# FORMATTED
~~~roc
module [foo]

dbg "foo"
foo = ...
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 4:1 to 4:5

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.binop_equals
    (Expr.lookup "foo")
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
foo : Error
~~~
