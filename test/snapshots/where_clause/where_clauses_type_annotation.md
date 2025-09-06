# META
~~~ini
description=Simple type annotation with where clause
type=file
~~~
# SOURCE
~~~roc
module [convert]

convert : a -> b where module(a).to_b : a -> b
convert = |a| a.to_b()
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent OpenRound CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "convert")
))
(block
  (binop_colon
    (lc "convert")
    (binop_arrow_call
      (binop_where
        (binop_arrow_call
          (lc "a")
          (lc "b")
        )
        (binop_colon
          (binop_pipe
            (apply_module
              (lc "a")
            )
            (dot_lc "to_b")
          )
          (lc "a")
        )
      )
      (lc "b")
    )
  )
  (binop_equals
    (lc "convert")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (lc "a")
            (dot_lc "to_b")
          )
        )
      )
      (args
        (lc "a")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [convert]

convert : a -> b where module(a).to_b : a -> b
convert = |a| a.to_b()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "convert"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "convert"))
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
convert : _arg -> _ret
a : _c
~~~
