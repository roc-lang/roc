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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport LowerIdent Dot UpperIdent LowerIdent OpBang OpColon UpperIdent OpFatArrow OpenCurly CloseCurly LowerIdent OpBang OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound LowerIdent OpBang OpAssign LowerIdent OpBang OpenRound String CloseRound ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "pf")
    (uc "Stdout")
  )
  (lc "print_msg")
  (unary_not <unary>)
  (uc "Str")
  (malformed malformed:expr_unexpected_token)
  (record_literal)
  (lc "print_msg")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (lc "msg")
    )
    (binop_pipe
      (uc "Stdout")
      (dot_lc "line")
    )
  )
  (unary_not <unary>)
  (lc "main")
  (unary_not <unary>)
  (lc "print_msg")
  (unary_not <unary>)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
MODULE NOT FOUND - effectful_with_effectful_annotation.md:3:1:3:17
# PROBLEMS
**Parse Error**
at 6:12 to 6:12

**Parse Error**
at 6:18 to 6:18

**Parse Error**
at 7:12 to 7:12

**Parse Error**
at 9:7 to 9:7

**Unsupported Node**
at 3:1 to 3:17

**Unsupported Node**
at 6:12 to 6:12

**Pattern in Expression Context**
at 6:14 to 6:17

**Unsupported Node**
at 6:18 to 6:18

**Unsupported Node**
at 7:10 to 7:12

**Unsupported Node**
at 7:20 to 7:26

**Unsupported Node**
at 9:7 to 9:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.lookup "print_msg")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.record_literal
  )
  (Expr.lookup "print_msg")
  (Expr.lambda)
  (Expr.malformed)
  (Expr.lookup "main")
  (Expr.malformed)
  (Expr.lookup "print_msg")
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
