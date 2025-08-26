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
  (binop_equals
    (not_lc "print_number")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (uc "Stdout")
            (not_lc "line")
          )
          (lc "n")
        )
      )
      (args
        (lc "n")
      )
    )
  )
  (binop_equals
    (not_lc "process")
    (lambda
      (body
        (apply_anon
          (not_lc "print_number")
          (apply_lc
            (lc "multiply")
            (tuple_literal
              (lc "x")
              (num_literal_i32 2)
            )
          )
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (apply_anon
      (not_lc "process")
      (num_literal_i32 42)
    )
  )
)
~~~
# FORMATTED
~~~roc
app
{
	pf: "../basic-cli/platform.roc" platform [
		main,
	],
}

import pf.Stdout

# Pure function with no annotation
multiply = \(x, y) -> x * y
print_number! = \n -> Stdout.line!(n)
process! = \x -> print_number!(multiply((x, 2)))
main! = process!(42)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_plus)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
