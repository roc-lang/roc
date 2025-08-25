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
identity: (a -> a)
identity = \x -> x

needs_string: ((Str -> Str) -> Str)
needs_string = \f -> f(["hello"])
needs_string(identity)
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:16 to 2:22

**Unsupported Node**
at 3:16 to 3:20

**Unsupported Node**
at 5:22 to 5:40

**Unsupported Node**
at 6:20 to 6:24

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "identity")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "needs_string")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
identity : Error
needs_string : Error
~~~
