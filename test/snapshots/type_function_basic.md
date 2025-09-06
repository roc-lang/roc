# META
~~~ini
description=Basic function type canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

apply : (_a -> _b) -> _a -> _b
apply = |fn, x| fn(x)

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
    (lc "apply")
    (binop_arrow_call
      (binop_arrow_call
        (binop_arrow_call
          (lc "_a")
          (lc "_b")
        )
        (lc "_a")
      )
      (lc "_b")
    )
  )
  (binop_equals
    (lc "apply")
    (lambda
      (body
        (apply_lc
          (lc "fn")
          (lc "x")
        )
      )
      (args
        (lc "fn")
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

apply :
	((_a -> _b) -> _a) -> _b
apply = |fn, x| fn(x)
main! = |_| {}
~~~
# EXPECTED
PARSE ERROR - type_function_basic.md:3:26:3:28
PARSE ERROR - type_function_basic.md:3:29:3:31
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "apply"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "apply"))
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
(var #19 -> #32)
(var #20 _)
(var #21 _)
(var #22 -> #33)
(var #23 _)
(var #24 -> #36)
(var #25 _)
(var #26 -> #35)
(var #27 -> #36)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 fn_pure)
(var #33 fn_pure)
(var #34 _)
(var #35 {})
(var #36 fn_pure)
~~~
# TYPES
~~~roc
x : _a
apply : _arg, _arg2 -> _ret
main : _arg -> {}
fn : _a
~~~
