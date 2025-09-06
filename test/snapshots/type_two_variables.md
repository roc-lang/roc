# META
~~~ini
description=Two distinct type variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

swap : (a, b) -> (b, a)
swap = |(x, y)| (y, x)

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar OpenRound LowerIdent Comma LowerIdent CloseRound OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block)
      )
    )
))
(block
  (binop_colon
    (lc "swap")
    (binop_arrow_call
      (tuple_literal
        (lc "a")
        (lc "b")
      )
      (tuple_literal
        (lc "b")
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "swap")
    (lambda
      (body
        (tuple_literal
          (lc "y")
          (lc "x")
        )
      )
      (args
        (tuple_literal
          (lc "x")
          (lc "y")
        )
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (record_literal)
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/main.roc" platform [] }

swap : (a, b) -> (b, a)
swap = |x, y| (y, x)
main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "swap"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "swap"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 37
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
(var #16 -> #33)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 -> #32)
(var #23 -> #33)
(var #24 _)
(var #25 -> #36)
(var #26 _)
(var #27 -> #35)
(var #28 -> #36)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 tuple)
(var #33 fn_pure)
(var #34 _)
(var #35 {})
(var #36 fn_pure)
~~~
# TYPES
~~~roc
swap : _arg -> (_field, _field2)
main : _arg -> {}
x : _c
y : _c
~~~
