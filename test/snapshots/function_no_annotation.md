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
**UNDEFINED VARIABLE**
Nothing is named **Stdout.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**function_no_annotation.md:9:21:9:33:**
```roc
print_number! = |n| Stdout.line!(n)
```
                    ^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**function_no_annotation.md:9:27:9:33:**
```roc
print_number! = |n| Stdout.line!(n)
```
                          ^^^^^^


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
    (Expr.apply_ident)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
