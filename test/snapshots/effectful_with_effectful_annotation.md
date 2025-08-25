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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly KwImport LowerIdent Dot UpperIdent LowerIdent OpBang OpColon UpperIdent OpFatArrow OpenCurly CloseCurly LowerIdent OpBang OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound LowerIdent OpBang OpAssign LowerIdent OpBang OpenRound String CloseRound ~~~
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
app { pf: ("../basic-cli/platform.roc" platform [main]) }

import pf exposing [Stdout]

# Function with effectful annotation using fat arrow
print_msg<malformed>!Str
<malformed>
{  }
print_msg
(<malformed>! | msg) | (Stdout | .line)
msg!
main<malformed>!
print_msg"Hello, world!"!
~~~
# EXPECTED
NIL
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
  (Expr.unary_not)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.record_literal
  )
  (Expr.lookup "print_msg")
  (Expr.lambda)
  (Expr.unary_not)
  (Expr.lookup "main")
  (Expr.unary_not)
  (Expr.lookup "print_msg")
  (Expr.unary_not)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "[True, False]_others")
~~~
# TYPES
~~~roc
~~~
