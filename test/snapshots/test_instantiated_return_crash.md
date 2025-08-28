# META
~~~ini
description=Return type mismatch with instantiated function
type=expr
~~~
# SOURCE
~~~roc
{
    identity : a -> a
    identity = |x| x

    needs_string : ((Str -> Str) -> Str)
    needs_string = |f| f(["hello"])

    needs_string(identity)
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpColon OpenRound OpenRound UpperIdent OpArrow UpperIdent CloseRound OpArrow UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpenRound OpenSquare String CloseSquare CloseRound LowerIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "identity")
    (binop_thin_arrow
      (lc "a")
      (lc "a")
    )
  )
  (binop_equals
    (lc "identity")
    (lambda
      (body
        (lc "x")
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "needs_string")
    (binop_thin_arrow
      (binop_thin_arrow
        (uc "Str")
        (uc "Str")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "needs_string")
    (lambda
      (body
        (apply_lc
          (lc "f")
          (list_literal
            (str_literal_big "hello")
          )
        )
      )
      (args
        (lc "f")
      )
    )
  )
  (apply_lc
    (lc "needs_string")
    (lc "identity")
  )
)
~~~
# FORMATTED
~~~roc
identity : a -> a
identity = \x -> x
needs_string :
	(Str -> Str) -> Str
needs_string = \f -> f(["hello"])
needs_string(identity)
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
  (Expr.binop_thick_arrow)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
