# META
~~~ini
description=Nested type applications in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

processNested : List(Result(Str, Err)) -> List(Str)
processNested = |_list| ["one","two"]

main! = |_| processNested([])
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound CloseRound OpArrow UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenSquare String Comma String CloseSquare BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound OpenSquare CloseSquare CloseRound ~~~
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
    (lc "processNested")
    (binop_arrow_call
      (apply_uc
        (uc "List")
        (apply_uc
          (uc "Result")
          (tuple_literal
            (uc "Str")
            (uc "Err")
          )
        )
      )
      (apply_uc
        (uc "List")
        (uc "Str")
      )
    )
  )
  (binop_equals
    (lc "processNested")
    (lambda
      (body
        (list_literal
          (str_literal_small "one")
          (str_literal_small "two")
        )
      )
      (args
        (lc "_list")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (apply_lc
          (lc "processNested")
          (list_literal)
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

processNested : List Result(Str, Err) -> List Str
processNested = |_list| ["one", "two"]
main! = |_| processNested([])
~~~
# EXPECTED
UNDECLARED TYPE - type_app_nested.md:3:34:3:37
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "processNested"))
    (type type_18)
  )
  (Stmt.assign
    (pattern (Patt.ident "processNested"))
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
; Total type variables: 40
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
(var #20 -> #36)
(var #21 _)
(var #22 Str)
(var #23 Str)
(var #24 _)
(var #25 -> #36)
(var #26 _)
(var #27 -> #39)
(var #28 _)
(var #29 -> #38)
(var #30 _)
(var #31 _)
(var #32 -> #39)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 fn_pure)
(var #37 _)
(var #38 fn_pure)
(var #39 fn_pure)
~~~
# TYPES
~~~roc
processNested : _arg -> _ret
main : _arg -> _ret
_list : _a
~~~
