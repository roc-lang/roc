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
    (binop_exposing
      (uc "Module")
      (list_literal
        (uc "Pair")
      )
    )
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
**Unsupported Node**
at 3:1 to 3:30

**Pattern in Expression Context**
at 5:10 to 5:11

**Pattern in Expression Context**
at 6:7 to 6:8

**Pattern in Expression Context**
at 7:7 to 7:8

**Pattern in Expression Context**
at 7:10 to 7:11

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_equals
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
