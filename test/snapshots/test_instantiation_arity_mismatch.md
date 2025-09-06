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
OpenCurly LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LowerIdent OpenRound Int Comma Int CloseRound CloseCurly ~~~
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
identity : (a, b) -> (a, b)
identity = |pair| pair

identity((1, 2))
~~~
# EXPECTED
TYPE MISMATCH - test_instantiation_arity_mismatch.md:5:5:5:13
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "identity")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
    (Expr.lambda (canonicalized))
  )
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
