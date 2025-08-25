# META
~~~ini
description=Type mismatch - pure annotation with effectful body
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

import pf.Stdout

# This should be a type error: pure annotation but effectful body
bad_function : Str -> {}
bad_function = |msg| Stdout.line!(msg)

main! = bad_function("This should fail")
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly KwImport LowerIdent Dot UpperIdent LowerIdent OpColon UpperIdent OpArrow OpenCurly CloseCurly LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound LowerIdent OpBang OpAssign LowerIdent OpenRound String CloseRound ~~~
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
app { pf: ("../basic-cli/platform.roc" platform [main]) }

import pf exposing [Stdout]

# This should be a type error: pure annotation but effectful body
bad_function: (Str -> {  })
bad_function = \msg -> Stdout | .line
msg!
main<malformed>!
bad_function("This should fail")
~~~
# EXPECTED
NIL
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
