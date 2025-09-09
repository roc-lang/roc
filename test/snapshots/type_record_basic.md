# META
~~~ini
description=Basic record type canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getName : { name: Str, age: U64 } -> Str
getName = |_person| "hello"

main! = |_| getName({name: "luke", age:21})
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly CloseRound ~~~
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
    (lc "getName")
    (binop_arrow_call
      (record_literal
        (binop_colon
          (lc "name")
          (uc "Str")
        )
        (binop_colon
          (lc "age")
          (uc "U64")
        )
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "getName")
    (lambda
      (body
        (str_literal_big "hello")
      )
      (args
        (lc "_person")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (apply_lc
          (lc "getName")
          (record_literal
            (binop_colon
              (lc "name")
              (str_literal_small "luke")
            )
            (binop_colon
              (lc "age")
              (num_literal_i32 21)
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

getName : {name: Str, age: U64} -> Str
getName = |_person| "hello"
main! = |_| getName({ name: "luke", age: 21 })
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_record_basic.md:3:1:3:8:**
```roc
getName : { name: Str, age: U64 } -> Str
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_record_basic.md:4:1:4:8:**
```roc
getName = |_person| "hello"
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_record_basic.md:6:1:6:6:**
```roc
main! = |_| getName({name: "luke", age:21})
```
^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "getName"))
    (type type_16)
  )
  (Stmt.assign
    (pattern (Patt.ident "getName"))
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
; Total type variables: 44
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
(var #18 -> #38)
(var #19 _)
(var #20 Str)
(var #21 -> #38)
(var #22 _)
(var #23 -> #43)
(var #24 _)
(var #25 -> #42)
(var #26 _)
(var #27 Str)
(var #28 _)
(var #29 _)
(var #30 Num *)
(var #31 _)
(var #32 -> #41)
(var #33 _)
(var #34 -> #43)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 fn_pure)
(var #39 _)
(var #40 {})
(var #41 record)
(var #42 fn_pure)
(var #43 fn_pure)
~~~
# TYPES
~~~roc
getName : _arg -> Str
main : _arg -> _ret
_person : _a
~~~
