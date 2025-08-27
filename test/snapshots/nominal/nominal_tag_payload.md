# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module [Maybe, some1, none1, some2, none2]

Maybe(a) := [Some(a), None]

some1 : a -> Maybe(a)
some1 = |a| Maybe.Some(a)

none1 : Maybe(_a)
none1 = Maybe.None

some2 = |a| Maybe.Some(a)

none2 = Maybe.None
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColonEqual OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent CloseSquare LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon_equals
    (apply_uc
      (uc "Maybe")
      (lc "a")
    )
    (list_literal
      (apply_uc
        (uc "Some")
        (lc "a")
      )
      (uc "None")
    )
  )
  (binop_colon
    (lc "some1")
    (binop_thin_arrow
      (lc "a")
      (apply_uc
        (uc "Maybe")
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "some1")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (uc "Maybe")
            (uc "Some")
          )
          (lc "a")
        )
      )
      (args
        (lc "a")
      )
    )
  )
  (binop_colon
    (lc "none1")
    (apply_uc
      (uc "Maybe")
      (lc "_a")
    )
  )
  (binop_equals
    (lc "none1")
    (binop_pipe
      (uc "Maybe")
      (uc "None")
    )
  )
  (binop_equals
    (lc "some2")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (uc "Maybe")
            (uc "Some")
          )
          (lc "a")
        )
      )
      (args
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "none2")
    (binop_pipe
      (uc "Maybe")
      (uc "None")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [
	Maybe,
	some1,
	none1,
	some2,
	none2,
]

Maybe(a) := [Some(a), None]

some1 : a -> Maybe a
some1 = \a -> Maybe.Some(a)

none1 : Maybe _a
none1 = Maybe.None

some2 = \a -> Maybe.Some(a)

none2 = Maybe.None
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
