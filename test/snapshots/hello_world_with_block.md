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
    (lc "pf")
    (uc "Stdout")
  )
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (block
      (binop_equals
        (lc "world")
        (str_literal_big "World")
      )
      (binop_pipe
        (uc "Stdout")
        (dot_lc "line")
      )
      (unary_not <unary>)
    )
  )
)
~~~
# FORMATTED
~~~roc
# Hello world!
# Multiline comments?
app { # Hello world!
# Multiline comments?
pf: ("../basic-cli/platform.roc" platform [main]) }

import pf exposing [Stdout]

main
(<malformed>! | _) | {
	world = "World"
	# Hello
Stdout | .line
	"Hello, world!"!
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 8:7 to 8:7

**Unsupported Node**
at 6:1 to 6:17

**Unsupported Node**
at 8:5 to 8:7

**Unsupported Node**
at 11:2 to 11:8

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.lookup "main")
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_arg, _arg2 -> [True, False]_others")
~~~
# TYPES
~~~roc
~~~
