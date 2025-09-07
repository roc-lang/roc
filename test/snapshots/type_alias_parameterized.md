# META
~~~ini
description=Parameterized type alias with type variables
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

Pair(a, b) : (a, b)

swapPair : Pair(a, b) -> Pair(b, a)
swapPair = |(x, y)| (y, x)

main! = |_| swapPair(1, 2)
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar OpenRound LowerIdent Comma LowerIdent CloseRound OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound Int Comma Int CloseRound ~~~
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
    (apply_uc
      (uc "Pair")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (tuple_literal
      (lc "a")
      (lc "b")
    )
  )
  (binop_colon
    (lc "swapPair")
    (binop_arrow_call
      (apply_uc
        (uc "Pair")
        (tuple_literal
          (lc "a")
          (lc "b")
        )
      )
      (apply_uc
        (uc "Pair")
        (tuple_literal
          (lc "b")
          (lc "a")
        )
      )
    )
  )
  (binop_equals
    (lc "swapPair")
    (lambda
      (body
        (tuple_literal
          (lc "y")
          (lc "x")
        )
      )
      (args
        (tuple_literal
          (lc "x")
          (lc "y")
        )
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (apply_lc
          (lc "swapPair")
          (tuple_literal
            (num_literal_i32 1)
            (num_literal_i32 2)
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

Pair((a, b)) : (a, b)
swapPair : Pair(a, b) -> Pair(b, a)
swapPair = |x, y| (y, x)
main! = |_| swapPair((1, 2))
~~~
# EXPECTED
TYPE MISMATCH - type_alias_parameterized.md:8:13:8:21
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_alias_parameterized.md:5:1:5:9:**
```roc
swapPair : Pair(a, b) -> Pair(b, a)
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_alias_parameterized.md:6:1:6:9:**
```roc
swapPair = |(x, y)| (y, x)
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_alias_parameterized.md:8:1:8:6:**
```roc
main! = |_| swapPair(1, 2)
```
^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "swapPair"))
    (type type_27)
  )
  (Stmt.assign
    (pattern (Patt.ident "swapPair"))
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
; Total type variables: 55
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
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 -> #50)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 -> #49)
(var #36 -> #50)
(var #37 _)
(var #38 -> #54)
(var #39 _)
(var #40 -> #53)
(var #41 Num *)
(var #42 Num *)
(var #43 -> #52)
(var #44 _)
(var #45 -> #54)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 tuple)
(var #50 fn_pure)
(var #51 _)
(var #52 tuple)
(var #53 fn_pure)
(var #54 fn_pure)
~~~
# TYPES
~~~roc
y : _c
main : _arg -> _ret
swapPair : _arg -> (_field, _field2)
x : _c
~~~
