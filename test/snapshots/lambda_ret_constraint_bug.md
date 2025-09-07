# META
~~~ini
description=Lambda return type constraint bug - integer literals in lambda bodies should be constrained by function signature
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "platform/main.roc" }

helper : I64 -> I64
helper = |n| n * 2

main : I64, I64 -> I64
main = |_, _| helper(5)
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int BlankLine LowerIdent OpColon UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar Underscore Comma Underscore OpBar LowerIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "platform/main.roc")
        (block)
      )
    )
))
(block
  (binop_colon
    (lc "helper")
    (binop_arrow_call
      (uc "I64")
      (uc "I64")
    )
  )
  (binop_equals
    (lc "helper")
    (lambda
      (body
        (binop_star
          (lc "n")
          (num_literal_i32 2)
        )
      )
      (args
        (lc "n")
      )
    )
  )
  (binop_colon
    (lc "main")
    (binop_arrow_call
      (uc "I64")
      (binop_arrow_call
        (uc "I64")
        (uc "I64")
      )
    )
  )
  (binop_equals
    (lc "main")
    (lambda
      (body
        (apply_lc
          (lc "helper")
          (num_literal_i32 5)
        )
      )
      (args
        (underscore)
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app [main] { pf: "platform/main.roc" platform [] }

helper : I64 -> I64
helper = |n| n * 2
main : I64 -> I64 -> I64
main = |_, _| helper(5)
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**lambda_ret_constraint_bug.md:3:1:3:7:**
```roc
helper : I64 -> I64
```
^^^^^^


**SHADOWING**
This definition shadows an existing one.

**lambda_ret_constraint_bug.md:4:1:4:7:**
```roc
helper = |n| n * 2
```
^^^^^^


**SHADOWING**
This definition shadows an existing one.

**lambda_ret_constraint_bug.md:7:1:7:5:**
```roc
main = |_, _| helper(5)
```
^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "helper"))
    (type type_10)
  )
  (Stmt.assign
    (pattern (Patt.ident "helper"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "main"))
    (type type_24)
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 42
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
(var #12 -> #36)
(var #13 _)
(var #14 -> #15)
(var #15 -> #16)
(var #16 Num *)
(var #17 -> #36)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 -> #41)
(var #27 _)
(var #28 _)
(var #29 -> #39)
(var #30 Num *)
(var #31 _)
(var #32 -> #41)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 fn_pure)
(var #37 _)
(var #38 _)
(var #39 fn_pure)
(var #40 fn_pure)
(var #41 fn_pure)
~~~
# TYPES
~~~roc
helper : _arg -> Num(_size)
n : _a
main : _arg -> _arg2 -> _ret
~~~
