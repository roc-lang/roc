# META
~~~ini
description=Multiple type arguments application in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

processDict : Dict(Str, U64) -> List(Str)
processDict = |_dict| []

main! = |_| processDict(Dict.empty().insert("one", 1))
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound OpArrow UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenSquare CloseSquare BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound UpperIdent Dot LowerIdent OpenRound CloseRound Dot LowerIdent OpenRound String Comma Int CloseRound CloseRound ~~~
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
    (lc "processDict")
    (binop_arrow_call
      (apply_uc
        (uc "Dict")
        (tuple_literal
          (uc "Str")
          (uc "U64")
        )
      )
      (apply_uc
        (uc "List")
        (uc "Str")
      )
    )
  )
  (binop_equals
    (lc "processDict")
    (lambda
      (body
        (list_literal)
      )
      (args
        (lc "_dict")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (apply_lc
          (lc "processDict")
          (apply_anon
            (binop_dot
              (apply_anon
                (binop_dot
                  (uc "Dict")
                  (dot_lc "empty")
                )
              )
              (dot_lc "insert")
            )
            (tuple_literal
              (str_literal_small "one")
              (num_literal_i32 1)
            )
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

processDict : Dict(Str, U64) -> List Str
processDict = |_dict| []
main! = |_| processDict(Dict..empty()..insert(("one", 1)))
~~~
# EXPECTED
UNDEFINED VARIABLE - type_app_multiple_args.md:6:25:6:35
TOO MANY ARGS - type_app_multiple_args.md:3:15:3:29
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_app_multiple_args.md:3:1:3:12:**
```roc
processDict : Dict(Str, U64) -> List(Str)
```
^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_app_multiple_args.md:4:1:4:12:**
```roc
processDict = |_dict| []
```
^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "processDict"))
    (type type_16)
  )
  (Stmt.assign
    (pattern (Patt.ident "processDict"))
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
(var #18 -> #43)
(var #19 _)
(var #20 -> #42)
(var #21 -> #43)
(var #22 _)
(var #23 -> #49)
(var #24 _)
(var #25 -> #48)
(var #26 _)
(var #27 _)
(var #28 -> #45)
(var #29 _)
(var #30 _)
(var #31 -> #47)
(var #32 Str)
(var #33 Num *)
(var #34 -> #46)
(var #35 _)
(var #36 _)
(var #37 -> #49)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 List #41)
(var #43 fn_pure)
(var #44 _)
(var #45 fn_pure)
(var #46 tuple)
(var #47 fn_pure)
(var #48 fn_pure)
(var #49 fn_pure)
~~~
# TYPES
~~~roc
processDict : _arg -> List(_elem)
main : _arg -> _ret
_dict : _a
~~~
