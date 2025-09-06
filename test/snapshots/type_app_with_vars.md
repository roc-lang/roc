# META
~~~ini
description=Type application with variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

mapList : List(a), (a -> b) -> List(b)
mapList = |list, fn| list.map(fn)

main! = |_| mapList([1,2,3,4,5])
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent Dot LowerIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound OpenSquare Int Comma Int Comma Int Comma Int Comma Int CloseSquare CloseRound ~~~
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
    (lc "mapList")
    (binop_arrow_call
      (apply_uc
        (uc "List")
        (lc "a")
      )
      (binop_arrow_call
        (binop_arrow_call
          (lc "a")
          (lc "b")
        )
        (apply_uc
          (uc "List")
          (lc "b")
        )
      )
    )
  )
  (binop_equals
    (lc "mapList")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (lc "list")
            (dot_lc "map")
          )
          (lc "fn")
        )
      )
      (args
        (lc "list")
        (lc "fn")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (apply_lc
          (lc "mapList")
          (list_literal
            (num_literal_i32 1)
            (num_literal_i32 2)
            (num_literal_i32 3)
            (num_literal_i32 4)
            (num_literal_i32 5)
          )
        )
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

mapList : List a -> (a -> b) -> List b
mapList = |list, fn| list.map(fn)
main! = |_| mapList([1, 2, 3, 4, 5])
~~~
# EXPECTED
TYPE MISMATCH - type_app_with_vars.md:6:13:6:20
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "mapList"))
    (type type_18)
  )
  (Stmt.assign
    (pattern (Patt.ident "mapList"))
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
; Total type variables: 50
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
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 -> #46)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 -> #45)
(var #26 _)
(var #27 _)
(var #28 -> #46)
(var #29 _)
(var #30 -> #49)
(var #31 _)
(var #32 -> #48)
(var #33 Num *)
(var #34 Num *)
(var #35 Num *)
(var #36 Num *)
(var #37 Num *)
(var #38 _)
(var #39 _)
(var #40 -> #49)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 fn_pure)
(var #46 fn_pure)
(var #47 _)
(var #48 fn_pure)
(var #49 fn_pure)
~~~
# TYPES
~~~roc
mapList : _arg, _arg2 -> _ret
main : _arg -> _ret
fn : _c
list : _c
~~~
