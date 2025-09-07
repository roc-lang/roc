# META
~~~ini
description=Multi-argument function type in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

curry : (_a, _b -> _c) -> (_a -> _b -> _c)
curry = |fn| |x| |y| fn(x, y)

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent OpArrow LowerIdent CloseRound OpArrow OpenRound LowerIdent OpArrow LowerIdent OpArrow LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpBar LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
    (lc "curry")
    (binop_arrow_call
      (binop_arrow_call
        (lc "_a")
        (binop_arrow_call
          (lc "_b")
          (lc "_c")
        )
      )
      (binop_arrow_call
        (binop_arrow_call
          (lc "_a")
          (lc "_b")
        )
        (lc "_c")
      )
    )
  )
  (binop_equals
    (lc "curry")
    (lambda
      (body
        (lambda
          (body
            (lambda
              (body
                (apply_lc
                  (lc "fn")
                  (tuple_literal
                    (lc "x")
                    (lc "y")
                  )
                )
              )
              (args
                (lc "y")
              )
            )
          )
          (args
            (lc "x")
          )
        )
      )
      (args
        (lc "fn")
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

curry :
	(_a -> _b -> _c) -> (_a -> _b) -> _c
curry = |fn| |x| |y| fn((x, y))
main! = |_| {}
~~~
# EXPECTED
PARSE ERROR - type_function_multi_arg.md:3:27:3:28
PARSE ERROR - type_function_multi_arg.md:3:40:3:42
PARSE ERROR - type_function_multi_arg.md:3:42:3:43
MALFORMED TYPE - type_function_multi_arg.md:3:27:3:39
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_function_multi_arg.md:3:1:3:6:**
```roc
curry : (_a, _b -> _c) -> (_a -> _b -> _c)
```
^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_function_multi_arg.md:4:1:4:6:**
```roc
curry = |fn| |x| |y| fn(x, y)
```
^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_function_multi_arg.md:6:1:6:6:**
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
    (pattern (Patt.ident "curry"))
    (type type_18)
  )
  (Stmt.assign
    (pattern (Patt.ident "curry"))
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
; Total type variables: 50
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
(var #20 -> #46)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 -> #43)
(var #25 _)
(var #26 _)
(var #27 -> #42)
(var #28 _)
(var #29 -> #44)
(var #30 -> #45)
(var #31 -> #46)
(var #32 _)
(var #33 -> #49)
(var #34 _)
(var #35 -> #48)
(var #36 -> #49)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 tuple)
(var #43 fn_pure)
(var #44 fn_pure)
(var #45 fn_pure)
(var #46 fn_pure)
(var #47 _)
(var #48 {})
(var #49 fn_pure)
~~~
# TYPES
~~~roc
fn : _a
curry : _arg -> _arg2 -> _arg3 -> _ret
main : _arg -> {}
x : _a
y : _a
~~~
