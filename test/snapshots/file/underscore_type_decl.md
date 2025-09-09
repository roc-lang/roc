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
KwModule OpenSquare CloseSquare BlankLine KwImport UpperIdent KwExposing OpenSquare UpperIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent Comma Underscore CloseRound OpAssign UpperIdent OpenRound Int Comma Int CloseRound UpperIdent OpenRound Underscore Comma LowerIdent CloseRound OpAssign UpperIdent OpenRound Int Comma Int CloseRound UpperIdent OpenRound Underscore Comma Underscore CloseRound OpAssign UpperIdent OpenRound Int Comma Int CloseRound ~~~
# PARSE
~~~clojure
(module-header)
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
PARSE ERROR - underscore_type_decl.md:8:1:8:1
MODULE NOT FOUND - underscore_type_decl.md:3:1:3:30
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.assign
    (pattern (Patt.applied_tag))
    (Expr.tag_applied)
  )
  (Stmt.assign
    (pattern (Patt.applied_tag))
    (Expr.tag_applied)
  )
  (Stmt.assign
    (pattern (Patt.applied_tag))
    (Expr.tag_applied)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 46
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 -> #15)
(var #11 -> #41)
(var #12 Num *)
(var #13 Num *)
(var #14 -> #40)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 -> #26)
(var #22 -> #43)
(var #23 Num *)
(var #24 Num *)
(var #25 -> #42)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 -> #37)
(var #33 -> #45)
(var #34 Num *)
(var #35 Num *)
(var #36 -> #44)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 tuple)
(var #41 fn_pure)
(var #42 tuple)
(var #43 fn_pure)
(var #44 tuple)
(var #45 fn_pure)
~~~
# TYPES
~~~roc
x : _a
y : _a
~~~
