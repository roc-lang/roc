# META
~~~ini
description=Function with no type annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

# Pure function with no annotation
multiply = |x, y| x * y

# Function with no type annotation - should infer effectfulness from body
print_number! = |n| Stdout.line!(n)

# Another effectful function with no annotation
process! = |x| print_number!(multiply(x, 2))

main! = process!(42)
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpStar LowerIdent BlankLine LineComment LowerIdent OpBang OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound BlankLine LineComment LowerIdent OpBang OpAssign OpBar LowerIdent OpBar LowerIdent OpBang OpenRound LowerIdent OpenRound LowerIdent Comma Int CloseRound CloseRound BlankLine LowerIdent OpBang OpAssign LowerIdent OpBang OpenRound Int CloseRound ~~~
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
  (binop_equals
    (lc "multiply")
    (lambda
      (body
        (binop_star
          (lc "x")
          (lc "y")
        )
      )
      (args
        (lc "x")
        (lc "y")
      )
    )
  )
  (binop_equals
    (not_lc "print_number")
    (lambda
      (body
        (apply_anon
          (binop_dot
            (uc "Stdout")
            (not_lc "line")
          )
          (lc "n")
        )
      )
      (args
        (lc "n")
      )
    )
  )
  (binop_equals
    (not_lc "process")
    (lambda
      (body
        (apply_anon
          (not_lc "print_number")
          (apply_lc
            (lc "multiply")
            (tuple_literal
              (lc "x")
              (num_literal_i32 2)
            )
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
    (apply_anon
      (not_lc "process")
      (num_literal_i32 42)
    )
  )
)
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

import pf.Stdout
# Pure function with no annotation
multiply = |x, y| x * y
# Function with no type annotation - should infer effectfulness from body
print_number! = |n| Stdout.line!(n)
# Another effectful function with no annotation
process! = |x| print_number!(multiply((x, 2)))
main! = process!(42)
~~~
# EXPECTED
MODULE NOT FOUND - function_no_annotation.md:3:1:3:17
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**function_no_annotation.md:6:1:6:9:**
```roc
multiply = |x, y| x * y
```
^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**function_no_annotation.md:9:27:9:33:**
```roc
print_number! = |n| Stdout.line!(n)
```
                          ^^^^^^


**SHADOWING**
This definition shadows an existing one.

**function_no_annotation.md:9:1:9:14:**
```roc
print_number! = |n| Stdout.line!(n)
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**function_no_annotation.md:12:13:12:14:**
```roc
process! = |x| print_number!(multiply(x, 2))
```
            ^


**SHADOWING**
This definition shadows an existing one.

**function_no_annotation.md:12:1:12:9:**
```roc
process! = |x| print_number!(multiply(x, 2))
```
^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.assign
    (pattern (Patt.ident "multiply"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "print_number"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "process"))
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
; Total type variables: 57
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
(var #11 -> #47)
(var #12 _)
(var #13 _)
(var #14 -> #15)
(var #15 -> #16)
(var #16 _)
(var #17 -> #47)
(var #18 _)
(var #19 -> #50)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 -> #49)
(var #24 _)
(var #25 _)
(var #26 -> #50)
(var #27 _)
(var #28 -> #55)
(var #29 _)
(var #30 -> #54)
(var #31 -> #53)
(var #32 _)
(var #33 Num *)
(var #34 -> #52)
(var #35 _)
(var #36 _)
(var #37 -> #55)
(var #38 _)
(var #39 -> #42)
(var #40 -> #56)
(var #41 Num *)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 fn_pure)
(var #48 _)
(var #49 fn_pure)
(var #50 fn_pure)
(var #51 _)
(var #52 tuple)
(var #53 fn_pure)
(var #54 fn_pure)
(var #55 fn_pure)
(var #56 fn_pure)
~~~
# TYPES
~~~roc
x : _a
print_number : _arg -> _ret
y : _a
main : _a
process : _arg -> _ret
n : _a
multiply : _arg, _arg2 -> _ret
~~~
