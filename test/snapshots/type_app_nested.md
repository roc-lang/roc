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
**SHADOWING**
This definition shadows an existing one.

**type_app_nested.md:3:1:3:14:**
```roc
processNested : List(Result(Str, Err)) -> List(Str)
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_app_nested.md:4:1:4:14:**
```roc
processNested = |_list| ["one","two"]
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_app_nested.md:6:1:6:6:**
```roc
main! = |_| processNested([])
```
^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



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
; Total type variables: 43
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
(var #20 -> #37)
(var #21 _)
(var #22 Str)
(var #23 -> #22)
(var #24 -> #36)
(var #25 -> #37)
(var #26 _)
(var #27 -> #42)
(var #28 _)
(var #29 -> #41)
(var #30 -> #40)
(var #31 _)
(var #32 -> #42)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 List #22)
(var #37 fn_pure)
(var #38 _)
(var #39 _)
(var #40 List #39)
(var #41 fn_pure)
(var #42 fn_pure)
~~~
# TYPES
~~~roc
processNested : _arg -> List(Str)
main : _arg -> _ret
_list : _a
~~~
