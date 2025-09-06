# META
~~~ini
description=Multiple uses of same type variable in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

pair : a -> (a, a)
pair = |x| (x, x)

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon LowerIdent OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
    (lc "pair")
    (binop_arrow_call
      (lc "a")
      (tuple_literal
        (lc "a")
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "pair")
    (lambda
      (body
        (tuple_literal
          (lc "x")
          (lc "x")
        )
      )
      (args
        (lc "x")
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

pair : a -> (a, a)
pair = |x| (x, x)
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
    (pattern (Patt.ident "pair"))
    (type type_12)
  )
  (Stmt.assign
    (pattern (Patt.ident "pair"))
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
; Total type variables: 33
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
(var #14 -> #29)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 -> #28)
(var #19 -> #29)
(var #20 _)
(var #21 -> #32)
(var #22 _)
(var #23 -> #31)
(var #24 -> #32)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 tuple)
(var #29 fn_pure)
(var #30 _)
(var #31 {})
(var #32 fn_pure)
~~~
# TYPES
~~~roc
pair : _arg -> (_field, _field2)
main : _arg -> {}
x : _b
~~~
