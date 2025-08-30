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
(module-header
  (exposes
    (uc "Maybe")

    (lc "some1")

    (lc "none1")

    (lc "some2")

    (lc "none2")
))
~~~
# FORMATTED
~~~roc
module [Maybe, some1, none1, some2, none2]

Maybe(a) := [Some(a), None]
some1 : a -> Maybe a
some1 = |a| Maybe.Some(a)
none1 : Maybe _a
none1 = Maybe.None
some2 = |a| Maybe.Some(a)
none2 = Maybe.None
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
    (Expr.lookup "some1")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "some1")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "none1")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "none1")
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "some2")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "none2")
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
some1 : _b
none1 : _b
some2 : _b
none2 : _b
~~~
