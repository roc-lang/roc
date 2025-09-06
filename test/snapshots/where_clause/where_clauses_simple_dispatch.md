# META
~~~ini
description=Simple where clause with single constraint
type=file
~~~
# SOURCE
~~~roc
module [stringify]

stringify : a -> Str where module(a).to_str : a -> Str
stringify = |value| value.to_str()
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon LowerIdent OpArrow UpperIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent OpenRound CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "stringify")
))
(block
  (binop_colon
    (lc "stringify")
    (binop_arrow_call
      (binop_where
        (binop_arrow_call
          (lc "a")
          (uc "Str")
        )
        (binop_colon
          (binop_dot
            (apply_module
              (lc "a")
            )
            (dot_lc "to_str")
          )
          (lc "a")
        )
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "stringify")
    (lambda
      (body
        (apply_anon
          (binop_dot
            (lc "value")
            (dot_lc "to_str")
          )
        )
      )
      (args
        (lc "value")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [stringify]

stringify : a -> Str where module(a)..to_str : a -> Str
stringify = |value| value..to_str()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "stringify"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "stringify"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 28
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
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 -> #27)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 -> #26)
(var #21 _)
(var #22 -> #27)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 fn_pure)
(var #27 fn_pure)
~~~
# TYPES
~~~roc
value : _b
stringify : _arg -> _ret
~~~
