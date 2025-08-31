# META
~~~ini
description=Function with no type annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpStar LowerIdent BlankLine LineComment LowerIdent OpBang OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound BlankLine LineComment LowerIdent OpBang OpAssign OpBar LowerIdent OpBar LowerIdent OpBang OpenRound LowerIdent OpenRound LowerIdent Comma Int CloseRound CloseRound BlankLine LowerIdent OpBang OpAssign LowerIdent OpBang OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

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
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **Stdout.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**function_no_annotation.md:9:21:9:33:**
```roc
print_number! = |n| Stdout.line!(n)
```
                    ^^^^^^^^^^^^


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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
