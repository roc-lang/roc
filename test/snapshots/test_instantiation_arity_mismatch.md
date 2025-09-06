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
    (binop_arrow_call
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
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "identity"))
    (type type_8)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
    (Expr.lambda (canonicalized))
  )
  (Expr.fn_call)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 25
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
(var #10 -> #22)
(var #11 _)
(var #12 _)
(var #13 -> #22)
(var #14 _)
(var #15 -> #24)
(var #16 Num *)
(var #17 Num *)
(var #18 -> #23)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 fn_pure)
(var #23 tuple)
(var #24 fn_pure)
~~~
# TYPES
~~~roc
identity : _arg -> _ret
pair : _c
~~~
