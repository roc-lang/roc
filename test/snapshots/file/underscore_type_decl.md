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
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - underscore_type_decl.md:5:13:5:14
PARSE ERROR - underscore_type_decl.md:5:20:5:21
PARSE ERROR - underscore_type_decl.md:5:23:5:24
PARSE ERROR - underscore_type_decl.md:6:1:6:6
PARSE ERROR - underscore_type_decl.md:6:6:6:7
PARSE ERROR - underscore_type_decl.md:6:7:6:8
PARSE ERROR - underscore_type_decl.md:6:8:6:9
PARSE ERROR - underscore_type_decl.md:6:10:6:11
PARSE ERROR - underscore_type_decl.md:6:11:6:12
PARSE ERROR - underscore_type_decl.md:6:13:6:14
PARSE ERROR - underscore_type_decl.md:6:20:6:21
PARSE ERROR - underscore_type_decl.md:6:23:6:24
PARSE ERROR - underscore_type_decl.md:7:1:7:6
PARSE ERROR - underscore_type_decl.md:7:6:7:7
PARSE ERROR - underscore_type_decl.md:7:7:7:8
PARSE ERROR - underscore_type_decl.md:7:8:7:9
PARSE ERROR - underscore_type_decl.md:7:10:7:11
PARSE ERROR - underscore_type_decl.md:7:11:7:12
PARSE ERROR - underscore_type_decl.md:7:13:7:14
PARSE ERROR - underscore_type_decl.md:7:20:7:21
PARSE ERROR - underscore_type_decl.md:7:23:7:24
PARSE ERROR - underscore_type_decl.md:7:25:7:25
MODULE NOT FOUND - underscore_type_decl.md:3:1:3:30
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:29

**Unsupported Node**
at 5:1 to 5:13

**Unsupported Node**
at 6:1 to 6:13

**Unsupported Node**
at 7:1 to 7:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
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
