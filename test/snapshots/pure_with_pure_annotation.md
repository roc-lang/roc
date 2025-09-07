# META
~~~ini
description=Pure function with pure annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Function with pure annotation using thin arrow
add : I32, I32 -> I32
add = |x, y| { x: x, y: y }.x

# Another pure function that calls a pure function
double : I32 -> I32
double = |x| add(x, x)

main! = add(1, 2)
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly Dot LowerIdent BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign LowerIdent OpenRound Int Comma Int CloseRound ~~~
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
        (str_literal_big "../basic-cli/platform.roc")
        (block)
      )
    )
))
(block
  (binop_colon
    (lc "add")
    (binop_arrow_call
      (uc "I32")
      (binop_arrow_call
        (uc "I32")
        (uc "I32")
      )
    )
  )
  (binop_equals
    (lc "add")
    (lambda
      (body
        (binop_dot
          (record_literal
            (binop_colon
              (lc "x")
              (lc "x")
            )
            (binop_colon
              (lc "y")
              (lc "y")
            )
          )
          (dot_lc "x")
        )
      )
      (args
        (lc "x")
        (lc "y")
      )
    )
  )
  (binop_colon
    (lc "double")
    (binop_arrow_call
      (uc "I32")
      (uc "I32")
    )
  )
  (binop_equals
    (lc "double")
    (lambda
      (body
        (apply_lc
          (lc "add")
          (tuple_literal
            (lc "x")
            (lc "x")
          )
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (apply_lc
      (lc "add")
      (tuple_literal
        (num_literal_i32 1)
        (num_literal_i32 2)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

# Function with pure annotation using thin arrow
add : I32 -> I32 -> I32
add = |x, y| { x: x, y: y }..x
# Another pure function that calls a pure function
double : I32 -> I32
double = |x| add((x, x))
main! = add((1, 2))
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**pure_with_pure_annotation.md:4:1:4:4:**
```roc
add : I32, I32 -> I32
```
^^^


**SHADOWING**
This definition shadows an existing one.

**pure_with_pure_annotation.md:5:1:5:4:**
```roc
add = |x, y| { x: x, y: y }.x
```
^^^


**SHADOWING**
This definition shadows an existing one.

**pure_with_pure_annotation.md:8:1:8:7:**
```roc
double : I32 -> I32
```
^^^^^^


**SHADOWING**
This definition shadows an existing one.

**pure_with_pure_annotation.md:9:11:9:12:**
```roc
double = |x| add(x, x)
```
          ^


**SHADOWING**
This definition shadows an existing one.

**pure_with_pure_annotation.md:9:1:9:7:**
```roc
double = |x| add(x, x)
```
^^^^^^


**SHADOWING**
This definition shadows an existing one.

**pure_with_pure_annotation.md:11:1:11:6:**
```roc
main! = add(1, 2)
```
^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "add"))
    (type type_12)
  )
  (Stmt.assign
    (pattern (Patt.ident "add"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "double"))
    (type type_31)
  )
  (Stmt.assign
    (pattern (Patt.ident "double"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.fn_call)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 62
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
(var #14 -> #55)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 -> #53)
(var #24 _)
(var #25 _)
(var #26 -> #55)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 -> #59)
(var #34 _)
(var #35 -> #58)
(var #36 _)
(var #37 _)
(var #38 -> #57)
(var #39 _)
(var #40 -> #59)
(var #41 _)
(var #42 -> #47)
(var #43 -> #61)
(var #44 Num *)
(var #45 Num *)
(var #46 -> #60)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 {})
(var #53 record)
(var #54 fn_pure)
(var #55 fn_pure)
(var #56 _)
(var #57 tuple)
(var #58 fn_pure)
(var #59 fn_pure)
(var #60 tuple)
(var #61 fn_pure)
~~~
# TYPES
~~~roc
add : _arg -> _arg2 -> _ret
main : _a
x : _a
double : _arg -> _ret
y : _a
~~~
