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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport LowerIdent Dot UpperIdent LowerIdent OpColon UpperIdent OpArrow OpenCurly CloseCurly LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound LowerIdent OpBang OpAssign LowerIdent OpenRound String CloseRound ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "pf")
    (uc "Stdout")
  )
  (binop_colon
    (lc "bad_function")
    (binop_thin_arrow
      (uc "Str")
      (record_literal)
    )
  )
  (binop_equals
    (lc "bad_function")
    (lambda
      (body
        (binop_pipe
          (uc "Stdout")
          (dot_lc "line")
        )
      )
      (args
        (lc "msg")
      )
    )
  )
  (unary_not <unary>)
  (lc "main")
  (unary_not <unary>)
  (apply_lc
    (lc "bad_function")
    (str_literal_big "This should fail")
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
MODULE NOT FOUND - pure_annotation_effectful_body_error.md:3:1:3:17
# PROBLEMS
**Parse Error**
at 9:7 to 9:7

**Unsupported Node**
at 3:1 to 3:17

**Unsupported Node**
at 6:16 to 6:24

**Unsupported Node**
at 7:16 to 7:22

**Unsupported Node**
at 9:7 to 9:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "bad_function")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.unary_not)
  (Expr.lookup "main")
  (Expr.unary_not)
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
