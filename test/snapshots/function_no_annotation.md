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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport LowerIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpStar LowerIdent LowerIdent OpBang OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound LowerIdent OpBang OpAssign OpBar LowerIdent OpBar LowerIdent OpBang OpenRound LowerIdent OpenRound LowerIdent Comma Int CloseRound CloseRound LowerIdent OpBang OpAssign LowerIdent OpBang OpenRound Int CloseRound ~~~
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
NO CHANGE
~~~
# EXPECTED
MODULE NOT FOUND - function_no_annotation.md:3:1:3:17
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
  (Expr.malformed)
  (Expr.lookup "process")
  (Expr.lambda)
  (Expr.malformed)
  (Expr.lookup "main")
  (Expr.malformed)
  (Expr.lookup "process")
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
