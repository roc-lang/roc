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
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "nums")
))
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
module [nums]

nums : List
U8
~~~
# EXPECTED
PARSE ERROR - type_annotation_missing_parens.md:4:1:4:1
EXPOSED BUT NOT DEFINED - type_annotation_missing_parens.md:1:9:1:13
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "nums"))
    (type type_3)
  )
  (Expr.tag_no_args)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 7
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
~~~
# TYPES
~~~roc
nums : _a
~~~
