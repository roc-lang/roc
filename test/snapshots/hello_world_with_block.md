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
# Hello world!
# Multiline comments?
app
{
	# Hello world!
	# Multiline comments?
	# Hello world!
# Multiline comments?
pf: "../basic-cli/platform.roc" platform [
		main,
	],
}

import pf.Stdout

main! = \_ -> {
	world = "World"
	# Hello
Stdout.line!("Hello, world!")
}
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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
