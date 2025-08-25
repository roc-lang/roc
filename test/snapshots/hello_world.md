# META
~~~ini
description=Hello world
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

import pf.Stdout

main! = |_| Stdout.line!("Hello, world!")
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly KwImport LowerIdent Dot UpperIdent LowerIdent OpBang OpAssign OpBar Underscore OpBar UpperIdent Dot LowerIdent OpBang OpenRound String CloseRound ~~~
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
    (binop_pipe
      (uc "Stdout")
      (dot_lc "line")
    )
  )
  (unary_not <unary>)
)
~~~
# FORMATTED
~~~roc
app { pf: ("../basic-cli/platform.roc" platform [main]) }

import pf exposing [Stdout]

main
(<malformed>! | _) | (Stdout | .line)
"Hello, world!"!
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 5:7 to 5:7

**Unsupported Node**
at 3:1 to 3:17

**Unsupported Node**
at 5:5 to 5:7

**Unsupported Node**
at 5:13 to 5:19

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.lookup "main")
  (Expr.lambda)
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
