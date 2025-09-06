# META
~~~ini
description=Type mismatch - pure annotation with effectful body
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

# This should be a type error: pure annotation but effectful body
bad_function : Str -> {}
bad_function = |msg| Stdout.line!(msg)

main! = bad_function("This should fail")
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow OpenCurly CloseCurly LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign LowerIdent OpenRound String CloseRound ~~~
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

# This should be a type error: pure annotation but effectful body
bad_function : Str -> {}
bad_function = |msg| Stdout.line!(msg)

main! = bad_function("This should fail")
~~~
# EXPECTED
MODULE NOT FOUND - pure_annotation_effectful_body_error.md:3:1:3:17
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **Stdout.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**pure_annotation_effectful_body_error.md:7:22:7:34:**
```roc
bad_function = |msg| Stdout.line!(msg)
```
                     ^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**pure_annotation_effectful_body_error.md:7:28:7:34:**
```roc
bad_function = |msg| Stdout.line!(msg)
```
                           ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.type_anno
    (name "bad_function")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "bad_function"))
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
