# META
~~~ini
description=Type mismatch showing nominal type origin from different module
type=file
~~~
# SOURCE
~~~roc
module []

import Data exposing [Person]

expectsPerson : Person -> Str
expectsPerson = |p| "Got a person"

main =
    # This will cause a type mismatch
    expectsPerson("not a person")
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport UpperIdent KwExposing OpenSquare UpperIdent CloseSquare LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpAssign LowerIdent OpenRound String CloseRound ~~~
# PARSE
~~~clojure
(block
  (import
    (binop_exposing
      (uc "Data")
      (list_literal
        (uc "Person")
      )
    )
  )
  (binop_colon
    (lc "expectsPerson")
    (binop_thin_arrow
      (uc "Person")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "expectsPerson")
    (lambda
      (body
        (str_literal_big "Got a person")
      )
      (args
        (lc "p")
      )
    )
  )
  (binop_equals
    (lc "main")
    (apply_lc
      (lc "expectsPerson")
      (str_literal_big "not a person")
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

import Data exposing [Person]
expectsPerson : Person -> Str
expectsPerson = |p| "Got a person"
main = # This will cause a type mismatch
expectsPerson("not a person")
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:30

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "expectsPerson")
    (Expr.binop_thin_arrow)
  )
  (Expr.binop_equals
    (Expr.lookup "expectsPerson")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "main")
    (Expr.apply_ident)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
expectsPerson : _a
main : _a
~~~
