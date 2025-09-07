# META
~~~ini
description=Record with type variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getField : { field: a, other: _b } -> a
getField = |record| record.field

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
    (lc "getField")
    (binop_arrow_call
      (record_literal
        (binop_colon
          (lc "field")
          (lc "a")
        )
        (binop_colon
          (lc "other")
          (lc "_b")
        )
      )
      (lc "a")
    )
  )
  (binop_equals
    (lc "getField")
    (lambda
      (body
        (binop_dot
          (lc "record")
          (dot_lc "field")
        )
      )
      (args
        (lc "record")
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

getField : {field: a, other: _b} -> a
getField = |record| record..field
main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_record_with_vars.md:3:1:3:9:**
```roc
getField : { field: a, other: _b } -> a
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_record_with_vars.md:4:1:4:9:**
```roc
getField = |record| record.field
```
^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "getField"))
    (type type_16)
  )
  (Stmt.assign
    (pattern (Patt.ident "getField"))
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
; Total type variables: 36
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
(var #18 -> #32)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 -> #32)
(var #24 _)
(var #25 -> #35)
(var #26 _)
(var #27 -> #34)
(var #28 -> #35)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 fn_pure)
(var #33 _)
(var #34 {})
(var #35 fn_pure)
~~~
# TYPES
~~~roc
getField : _arg -> _ret
main : _arg -> {}
record : _b
~~~
