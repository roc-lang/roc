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
OpenCurly LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LowerIdent OpColon OpenRound OpenRound UpperIdent OpArrow UpperIdent CloseRound OpArrow UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpenRound OpenSquare String CloseSquare CloseRound BlankLine LowerIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "identity")
    (binop_arrow_call
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
    (binop_arrow_call
      (binop_arrow_call
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
identity = |x| x
needs_string :
	(Str -> Str) -> Str
needs_string = |f| f(["hello"])
needs_string(identity)
~~~
# EXPECTED
TYPE MISMATCH - test_instantiated_return_crash.md:6:26:6:35
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "identity"))
    (type type_4)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "needs_string"))
    (type type_16)
  )
  (Stmt.assign
    (pattern (Patt.ident "needs_string"))
    (Expr.lambda (canonicalized))
  )
  (Expr.fn_call)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 36
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 -> #31)
(var #7 _)
(var #8 _)
(var #9 -> #31)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 -> #34)
(var #19 _)
(var #20 -> #33)
(var #21 Str)
(var #22 _)
(var #23 _)
(var #24 -> #34)
(var #25 _)
(var #26 -> #35)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 fn_pure)
(var #32 _)
(var #33 fn_pure)
(var #34 fn_pure)
(var #35 fn_pure)
~~~
# TYPES
~~~roc
identity : _arg -> _ret
f : _b
needs_string : _arg -> _ret
x : _b
~~~
