# META
~~~ini
description=underscore_in_assignment_pattern
type=file
~~~
# SOURCE
~~~roc
module []

import Module exposing [Pair]

Pair1(x, _) = Pair(0, 1)
Pair2(_, y) = Pair(0, 1)
Pair3(_, _) = Pair(0, 1)
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport UpperIdent KwExposing OpenSquare UpperIdent CloseSquare UpperIdent OpenRound LowerIdent Comma Underscore CloseRound OpAssign UpperIdent OpenRound Int Comma Int CloseRound UpperIdent OpenRound Underscore Comma LowerIdent CloseRound OpAssign UpperIdent OpenRound Int Comma Int CloseRound UpperIdent OpenRound Underscore Comma Underscore CloseRound OpAssign UpperIdent OpenRound Int Comma Int CloseRound ~~~
# PARSE
~~~clojure
(block
  (import
    (uc "Module")
    (uc "Pair")
  )
  (binop_equals
    (apply_uc
      (uc "Pair1")
      (tuple_literal
        (lc "x")
        (underscore)
      )
    )
    (apply_uc
      (uc "Pair")
      (tuple_literal
        (num_literal_i32 0)
        (num_literal_i32 1)
      )
    )
  )
  (binop_equals
    (apply_uc
      (uc "Pair2")
      (tuple_literal
        (underscore)
        (lc "y")
      )
    )
    (apply_uc
      (uc "Pair")
      (tuple_literal
        (num_literal_i32 0)
        (num_literal_i32 1)
      )
    )
  )
  (binop_equals
    (apply_uc
      (uc "Pair3")
      (tuple_literal
        (underscore)
        (underscore)
      )
    )
    (apply_uc
      (uc "Pair")
      (tuple_literal
        (num_literal_i32 0)
        (num_literal_i32 1)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

import Module exposing [Pair]
Pair1((x, _)) = Pair((0, 1))
Pair2((_, y)) = Pair((0, 1))
Pair3((_, _)) = Pair((0, 1))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_plus)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
