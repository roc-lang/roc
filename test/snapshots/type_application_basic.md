# META
~~~ini
description=Basic type application canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

processList : List(Str) -> U64
processList = |list| list.len()

main! = |_| processList(["one","two","three"])
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent OpenRound CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound OpenSquare String Comma String Comma String CloseSquare CloseRound ~~~
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
    (lc "processList")
    (binop_arrow_call
      (apply_uc
        (uc "List")
        (uc "Str")
      )
      (uc "U64")
    )
  )
  (binop_equals
    (lc "processList")
    (lambda
      (body
        (apply_anon
          (binop_dot
            (lc "list")
            (dot_lc "len")
          )
        )
      )
      (args
        (lc "list")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (apply_lc
          (lc "processList")
          (list_literal
            (str_literal_small "one")
            (str_literal_small "two")
            (str_literal_big "three")
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

processList : List Str -> U64
processList = |list| list..len()
main! = |_| processList(["one", "two", "three"])
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_application_basic.md:3:1:3:12:**
```roc
processList : List(Str) -> U64
```
^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_application_basic.md:4:1:4:12:**
```roc
processList = |list| list.len()
```
^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "processList"))
    (type type_12)
  )
  (Stmt.assign
    (pattern (Patt.ident "processList"))
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
(var #14 -> #35)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 -> #34)
(var #19 _)
(var #20 -> #35)
(var #21 _)
(var #22 -> #39)
(var #23 _)
(var #24 -> #38)
(var #25 Str)
(var #26 -> #25)
(var #27 -> #25)
(var #28 -> #37)
(var #29 _)
(var #30 -> #39)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 fn_pure)
(var #35 fn_pure)
(var #36 _)
(var #37 List #25)
(var #38 fn_pure)
(var #39 fn_pure)
~~~
# TYPES
~~~roc
processList : _arg -> _ret
main : _arg -> _ret
list : _a
~~~
