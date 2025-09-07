# META
~~~ini
description=Higher-order function with multiple type variables
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)
compose = |f, g| |x| f(g(x))

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow OpenRound LowerIdent OpArrow LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent OpenRound LowerIdent CloseRound CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
    (lc "compose")
    (binop_arrow_call
      (binop_arrow_call
        (binop_arrow_call
          (lc "_b")
          (lc "_c")
        )
        (binop_arrow_call
          (lc "_a")
          (lc "_b")
        )
      )
      (binop_arrow_call
        (lc "_a")
        (lc "_c")
      )
    )
  )
  (binop_equals
    (lc "compose")
    (lambda
      (body
        (lambda
          (body
            (apply_lc
              (lc "f")
              (apply_lc
                (lc "g")
                (lc "x")
              )
            )
          )
          (args
            (lc "x")
          )
        )
      )
      (args
        (lc "f")
        (lc "g")
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

compose :
	((_b -> _c) -> _a -> _b) -> _a -> _c
compose = |f, g| |x| f(g(x))
main! = |_| {}
~~~
# EXPECTED
PARSE ERROR - type_higher_order_multiple_vars.md:3:36:3:38
PARSE ERROR - type_higher_order_multiple_vars.md:3:39:3:40
PARSE ERROR - type_higher_order_multiple_vars.md:3:40:3:42
PARSE ERROR - type_higher_order_multiple_vars.md:3:43:3:45
PARSE ERROR - type_higher_order_multiple_vars.md:3:46:3:48
PARSE ERROR - type_higher_order_multiple_vars.md:3:48:3:49
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_higher_order_multiple_vars.md:3:1:3:8:**
```roc
compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_higher_order_multiple_vars.md:4:1:4:8:**
```roc
compose = |f, g| |x| f(g(x))
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_higher_order_multiple_vars.md:6:1:6:6:**
```roc
main! = |_| {}
```
^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "compose"))
    (type type_18)
  )
  (Stmt.assign
    (pattern (Patt.ident "compose"))
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
; Total type variables: 49
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
(var #20 -> #45)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 -> #42)
(var #25 -> #41)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 -> #43)
(var #30 -> #45)
(var #31 _)
(var #32 -> #48)
(var #33 _)
(var #34 -> #47)
(var #35 -> #48)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 fn_pure)
(var #42 fn_pure)
(var #43 fn_pure)
(var #44 fn_pure)
(var #45 fn_pure)
(var #46 _)
(var #47 {})
(var #48 fn_pure)
~~~
# TYPES
~~~roc
g : _a
compose : _arg -> _arg2 -> _arg3 -> _ret
main : _arg -> {}
x : _a
f : _a
~~~
