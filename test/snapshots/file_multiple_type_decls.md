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
KwModule OpenSquare CloseSquare UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent OpenRound UpperIdent CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (uc "FirstType")
    (uc "U64")
  )
  (binop_colon
    (uc "SecondType")
    (uc "Str")
  )
  (binop_colon
    (uc "ThirdType")
    (apply_uc
      (uc "List")
      (uc "U8")
    )
  )
)
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
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
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
