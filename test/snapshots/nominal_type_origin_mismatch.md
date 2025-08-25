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
    (uc "Data")
    (uc "Person")
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
expectsPerson: (Person -> Str)
expectsPerson = \p -> "Got a person"

main = expectsPerson("not a person")
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:29

**Unsupported Node**
at 5:17 to 5:30

**Unsupported Node**
at 6:17 to 6:21

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "expectsPerson")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
