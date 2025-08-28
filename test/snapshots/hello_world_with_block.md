# META
~~~ini
description=Hello world with a block
type=file
~~~
# SOURCE
~~~roc
# Hello world!

# Multiline comments?
app { pf: "../basic-cli/platform.roc" platform [main!] }

import pf.Stdout

main! = |_| {
	world = "World"
	# Hello
	Stdout.line!("Hello, world!")
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly KwImport LowerIdent Dot UpperIdent LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign String UpperIdent Dot LowerIdent OpBang OpenRound String CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (import
    (binop_pipe
      (lc "pf")
      (uc "Stdout")
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (block
          (binop_equals
            (lc "world")
            (str_literal_big "World")
          )
          (apply_anon
            (binop_pipe
              (uc "Stdout")
              (not_lc "line")
            )
            (str_literal_big "Hello, world!")
          )
        )
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

import pf.Stdout
main! = |_| {
	world = "World"	# Hello
	Stdout.line!("Hello, world!")
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 6:1 to 6:17

**Unsupported Node**
at 11:2 to 11:8

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.not_lookup)
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
