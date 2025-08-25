# META
~~~ini
description=Polymorphic function instantiation with arity mismatch
type=expr
~~~
# SOURCE
~~~roc
{
    identity : (a, b) -> (a, b)
    identity = |pair| pair

    identity(1, 2)
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpenRound Int Comma Int CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "identity")
    (binop_thin_arrow
      (tuple_literal
        (lc "a")
        (lc "b")
      )
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
  )
  (binop_equals
    (lc "identity")
    (lambda
      (body
        (lc "pair")
      )
      (args
        (lc "pair")
      )
    )
  )
  (apply_lc
    (lc "identity")
    (tuple_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
    )
  )
)
~~~
# FORMATTED
~~~roc
identity: ((a, b) -> (a, b))
identity = \pair -> pair

identity((1, 2))
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:21 to 2:32

**Unsupported Node**
at 3:16 to 3:23

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "identity")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
identity : Error
~~~
