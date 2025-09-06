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
(block
  (import
    (binop_pipe
      (lc "pf")
      (uc "Stdout")
    )
  )
  (binop_colon
    (lc "bad_function")
    (binop_arrow_call
      (uc "Str")
      (record_literal)
    )
  )
  (binop_equals
    (lc "bad_function")
    (lambda
      (body
        (apply_anon
          (binop_pipe
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
    (apply_lc
      (lc "bad_function")
      (str_literal_big "This should fail")
    )
  )
)
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
Nothing is named **pf** in this scope.
Is there an **import** or **exposing** missing up-top?

**pure_annotation_effectful_body_error.md:3:8:3:10:**
```roc
import pf.Stdout
```
       ^^


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
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "bad_function"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "bad_function"))
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
; Total type variables: 35
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
(var #34 fn_pure)
~~~
# TYPES
~~~roc
main : _a
msg : _a
bad_function : _arg -> _ret
~~~
