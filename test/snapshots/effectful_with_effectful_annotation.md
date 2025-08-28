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
  (binop_colon
    (not_lc "print_msg")
    (binop_thick_arrow
      (uc "Str")
      (record_literal)
    )
  )
  (binop_equals
    (not_lc "print_msg")
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
    (apply_anon
      (not_lc "print_msg")
      (str_literal_big "Hello, world!")
    )
  )
)
~~~
# FORMATTED
~~~roc
app
{
	pf: "../basic-cli/platform.roc" platform [main],
}

import pf.Stdout
print_msg! : Str => {  }
print_msg! = \msg -> Stdout.line!(msg)
main! = print_msg!("Hello, world!")
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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
