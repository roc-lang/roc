# META
~~~ini
description=Example of a recursive nominal tag union with payload
type=file
~~~
# SOURCE
~~~roc
module [ConsList, empty]

ConsList(a) := [Nil, Node(ConsList(a))]

empty : ConsList(_a)
empty = ConsList.Nil
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColonEqual OpenSquare UpperIdent Comma UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound CloseSquare LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon_equals
    (apply_uc
      (uc "ConsList")
      (lc "a")
    )
    (list_literal
      (uc "Nil")
      (apply_uc
        (uc "Node")
        (apply_uc
          (uc "ConsList")
          (lc "a")
        )
      )
    )
  )
  (binop_colon
    (lc "empty")
    (apply_uc
      (uc "ConsList")
      (lc "_a")
    )
  )
  (binop_equals
    (lc "empty")
    (binop_pipe
      (uc "ConsList")
      (uc "Nil")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [ConsList, empty]

ConsList(a) := [Nil, Node(ConsList(a))]
empty : ConsList _a
empty = ConsList.Nil
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
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (Expr.lookup "empty")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "empty")
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
empty : _b
~~~
