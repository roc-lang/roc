# META
~~~ini
description=Multiple unqualified type declarations
type=file
~~~
# SOURCE
~~~roc
module []

FirstType : U64
SecondType : Str
ThirdType : List(U8)
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent OpenRound UpperIdent CloseRound ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

FirstType : U64
SecondType : Str
ThirdType : List U8
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name node:uc)
    (type uc)
  )
  (Stmt.type_anno
    (name node:uc)
    (type uc)
  )
  (Stmt.type_anno
    (name node:uc)
    (type apply_uc)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
