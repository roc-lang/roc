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
module [
	ConsList,
	empty,
]

ConsList(a) := [Nil, Node(ConsList(a))]

empty: ConsList(_a)
empty = ConsList.Nil
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:40

**Unsupported Node**
at 6:9 to 6:17

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "empty")
    (Expr.apply_tag)
  )
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
