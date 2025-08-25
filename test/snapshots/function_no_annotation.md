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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly KwImport LowerIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpStar LowerIdent LowerIdent OpBang OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound LowerIdent OpBang OpAssign OpBar LowerIdent OpBar LowerIdent OpBang OpenRound LowerIdent OpenRound LowerIdent Comma Int CloseRound CloseRound LowerIdent OpBang OpAssign LowerIdent OpBang OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "pf")
    (uc "Stdout")
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
        (tuple_literal
          (lc "x")
          (lc "y")
        )
      )
    )
  )
  (lc "print_number")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (lc "n")
    )
    (binop_pipe
      (uc "Stdout")
      (dot_lc "line")
    )
  )
  (unary_not <unary>)
  (lc "process")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (lc "x")
    )
    (lc "print_number")
  )
  (unary_not <unary>)
  (lc "main")
  (unary_not <unary>)
  (lc "process")
  (unary_not <unary>)
)
~~~
# FORMATTED
~~~roc
app { pf: ("../basic-cli/platform.roc" platform [main]) }

import pf exposing [Stdout]

# Pure function with no annotation
multiply = \(x, y) -> x * y
print_number
(<malformed>! | n) | (Stdout | .line)
n!
process
(<malformed>! | x) | print_number
multiply((x, 2))!
main<malformed>!
process42!
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 9:15 to 9:15

**Parse Error**
at 12:10 to 12:10

**Parse Error**
at 14:7 to 14:7

**Unsupported Node**
at 3:1 to 3:17

**Unsupported Node**
at 6:12 to 6:19

**Unsupported Node**
at 9:13 to 9:15

**Unsupported Node**
at 9:21 to 9:27

**Unsupported Node**
at 12:8 to 12:10

**Unsupported Node**
at 14:7 to 14:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "print_number")
  (Expr.lambda)
  (Expr.unary_not)
  (Expr.lookup "process")
  (Expr.lambda)
  (Expr.unary_not)
  (Expr.lookup "main")
  (Expr.unary_not)
  (Expr.lookup "process")
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
