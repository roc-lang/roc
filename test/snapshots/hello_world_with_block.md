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
LineComment KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly KwImport LowerIdent Dot UpperIdent LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign String UpperIdent Dot LowerIdent OpBang OpenRound String CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (block
    (binop_colon
      (lc "pf")
      (str_literal_big "../basic-cli/platform.roc")
    )
    (malformed malformed:expr_unexpected_token)
    (list_literal
      (not_lc "main")
    )
  )
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
# Hello world!


# Multiline comments?
app {
	pf : "../basic-cli/platform.roc"
	platform 
	[main!]
}
import pf.Stdout
main! = |_| {
	world = "World"
	Stdout.line!("Hello, world!")
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:15 to 4:1

**Parse Error**
at 4:1 to 4:5

**Parse Error**
at 4:39 to 4:48

**Unsupported Node**
at 6:1 to 6:17

**Unsupported Node**
at 11:2 to 11:8

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.block
    (Expr.binop_colon
      (Expr.lookup "pf")
      (Expr.str_literal_big)
    )
    (Expr.malformed)
    (Expr.list_literal)
  )
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
