# META
~~~ini
description=Effectful function with effectful annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

# Function with effectful annotation using fat arrow
print_msg! : Str => {}
print_msg! = |msg| Stdout.line!(msg)

main! = print_msg!("Hello, world!")
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent BlankLine LineComment LowerIdent OpBang OpColon UpperIdent OpThinArrow OpenCurly CloseCurly LowerIdent OpBang OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign LowerIdent OpBang OpenRound String CloseRound ~~~
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
  (import
    (binop_dot
      (lc "pf")
      (uc "Stdout")
    )
  )
  (binop_colon
    (not_lc "print_msg")
    (binop_thick_arrow
      (uc "Str")
      (record_literal)
    )
  )
  (binop_equals
    (not_lc "print_msg")
    (lambda
      (body
        (apply_anon
          (binop_dot
            (uc "Stdout")
            (not_lc "line")
          )
          (lc "msg")
        )
      )
      (args
        (lc "msg")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (apply_anon
      (not_lc "print_msg")
      (str_literal_big "Hello, world!")
    )
  )
)
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

import pf.Stdout
# Function with effectful annotation using fat arrow
print_msg! : Str => {}
print_msg! = |msg| Stdout.line!(msg)
main! = print_msg!("Hello, world!")
~~~
# EXPECTED
MODULE NOT FOUND - effectful_with_effectful_annotation.md:3:1:3:17
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**effectful_with_effectful_annotation.md:6:1:6:11:**
```roc
print_msg! : Str => {}
```
^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**effectful_with_effectful_annotation.md:7:26:7:32:**
```roc
print_msg! = |msg| Stdout.line!(msg)
```
                         ^^^^^^


**SHADOWING**
This definition shadows an existing one.

**effectful_with_effectful_annotation.md:7:1:7:11:**
```roc
print_msg! = |msg| Stdout.line!(msg)
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**effectful_with_effectful_annotation.md:9:1:9:6:**
```roc
main! = print_msg!("Hello, world!")
```
^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "print_msg"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "print_msg"))
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
(var #16 -> #33)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 -> #32)
(var #21 _)
(var #22 _)
(var #23 -> #33)
(var #24 _)
(var #25 -> #28)
(var #26 -> #34)
(var #27 Str)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 fn_pure)
(var #33 fn_pure)
(var #34 -> #35)
(var #35 fn_pure)
~~~
# TYPES
~~~roc
main : _a
msg : _a
print_msg : _arg -> _ret
~~~
