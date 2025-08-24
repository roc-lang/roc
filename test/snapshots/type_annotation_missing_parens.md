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
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - type_annotation_missing_parens.md:3:15:3:15
EXPOSED BUT NOT DEFINED - type_annotation_missing_parens.md:1:9:1:13
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "nums")
    (Expr.apply_tag)
  )
  (Expr.apply_tag)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "[]_others")
~~~
# TYPES
~~~roc
~~~
