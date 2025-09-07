# META
~~~ini
description=Simple pure function type annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

identity : Str -> Str
identity = |x| x

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
    (lc "identity")
    (binop_arrow_call
      (uc "Str")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "identity")
    (lambda
      (body
        (lc "x")
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

identity : Str -> Str
identity = |x| x
main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_simple_pure.md:3:1:3:9:**
```roc
identity : Str -> Str
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_simple_pure.md:4:1:4:9:**
```roc
identity = |x| x
```
^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "identity"))
    (type type_10)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
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
; Total type variables: 28
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
(var #12 -> #24)
(var #13 _)
(var #14 _)
(var #15 -> #24)
(var #16 _)
(var #17 -> #27)
(var #18 _)
(var #19 -> #26)
(var #20 -> #27)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 fn_pure)
(var #25 _)
(var #26 {})
(var #27 fn_pure)
~~~
# TYPES
~~~roc
x : _a
identity : _arg -> _ret
main : _arg -> {}
~~~
