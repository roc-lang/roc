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
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Pattern in Expression Context**
at 3:1 to 3:10

**Pattern in Expression Context**
at 3:13 to 3:16

**Pattern in Expression Context**
at 4:1 to 4:11

**Pattern in Expression Context**
at 4:14 to 4:17

**Pattern in Expression Context**
at 5:1 to 5:10

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.apply_tag)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
