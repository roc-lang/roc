# META
~~~ini
description=Effectful function with effectful annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

import pf.Stdout

# Function with effectful annotation using fat arrow
print_msg! : Str => {}
print_msg! = |msg| Stdout.line!(msg)

main! = print_msg!("Hello, world!")
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent BlankLine LineComment LowerIdent OpBang OpColon UpperIdent OpFatArrow OpenCurly CloseCurly LowerIdent OpBang OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign LowerIdent OpBang OpenRound String CloseRound ~~~
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

# Function with effectful annotation using fat arrow
print_msg! : Str => {}
print_msg! = |msg| Stdout.line!(msg)

main! = print_msg!("Hello, world!")
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **Stdout.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**effectful_with_effectful_annotation.md:7:20:7:32:**
```roc
print_msg! = |msg| Stdout.line!(msg)
```
                   ^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.type_anno
    (name "print_msg")
    (type binop_thick_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "print_msg"))
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
