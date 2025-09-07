# META
~~~ini
description=Simple type alias usage in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

UserId : U64

getUser : UserId -> Str
getUser = |id| if (id > 10) "big" else "small"

main! = |_| getUser(100)
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine UpperIdent OpColon UpperIdent BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwIf OpenRound LowerIdent OpGreaterThan Int CloseRound String KwElse String BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound Int CloseRound ~~~
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
    (uc "UserId")
    (uc "U64")
  )
  (binop_colon
    (lc "getUser")
    (binop_arrow_call
      (uc "UserId")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "getUser")
    (lambda
      (body
        (if_else
          (condition             (binop_gt
              (lc "id")
              (num_literal_i32 10)
            )
)
          (then             (str_literal_small "big")
)
          (else             (str_literal_big "small")
))
      )
      (args
        (lc "id")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (apply_lc
          (lc "getUser")
          (num_literal_i32 100)
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

UserId : U64
getUser : UserId -> Str
getUser = |id| if id > 10 "big" else "small"
main! = |_| getUser(100)
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_alias_simple.md:5:1:5:8:**
```roc
getUser : UserId -> Str
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_alias_simple.md:6:1:6:8:**
```roc
getUser = |id| if (id > 10) "big" else "small"
```
^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "getUser"))
    (type type_13)
  )
  (Stmt.assign
    (pattern (Patt.ident "getUser"))
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
; Total type variables: 38
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
(var #15 -> #34)
(var #16 _)
(var #17 _)
(var #18 Num *)
(var #19 _)
(var #20 Str)
(var #21 Str)
(var #22 _)
(var #23 -> #34)
(var #24 _)
(var #25 -> #37)
(var #26 _)
(var #27 -> #36)
(var #28 Num *)
(var #29 _)
(var #30 -> #37)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 fn_pure)
(var #35 _)
(var #36 fn_pure)
(var #37 fn_pure)
~~~
# TYPES
~~~roc
main : _arg -> _ret
getUser : _arg -> _ret
id : _a
~~~
