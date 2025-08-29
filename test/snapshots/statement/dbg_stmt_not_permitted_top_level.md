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
(block
  (malformed malformed:expr_unexpected_token)
  (str_literal_small "foo")
  (binop_equals
    (lc "foo")
    (ellipsis)
  )
)
~~~
# FORMATTED
~~~roc
module [foo]

"foo"
foo = ...
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 4:1 to 4:1

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
