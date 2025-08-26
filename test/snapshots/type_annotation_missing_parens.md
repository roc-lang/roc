# META
~~~ini
description=Type annotation missing parentheses for type application
type=file
~~~
# SOURCE
~~~roc
module [nums]

nums : List U8
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpColon UpperIdent UpperIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "nums")
    (uc "List")
  )
  (uc "U8")
)
~~~
# FORMATTED
~~~roc
module [
	nums,
]

nums : List
U8
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.str_literal_small)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
