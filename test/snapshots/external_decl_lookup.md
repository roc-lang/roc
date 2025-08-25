# META
~~~ini
description=External declaration lookup from json module
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

import pf.Stdout
import json.Json

main! = |_| {
    # This should create an external declaration for json.Json.utf8
    result = Json.utf8("Hello from external module!")
    Stdout.line!(result)
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly KwImport LowerIdent Dot UpperIdent KwImport LowerIdent Dot UpperIdent LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound String CloseRound UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "pf")
    (uc "Stdout")
  )
  (import
    (lc "json")
    (uc "Json")
  )
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (block
      (binop_equals
        (lc "result")
        (apply_anon
          (binop_pipe
            (uc "Json")
            (dot_lc "utf8")
          )
          (str_literal_big "Hello from external module!")
        )
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
app { pf: ("../basic-cli/platform.roc" platform [main]) }

import pf exposing [Stdout]
import json exposing [Json]

main
(<malformed>! | _) | {
	result = Json | .utf8("Hello from external module!")
	Stdout | .line
	result!
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 6:7 to 6:7

**Unsupported Node**
at 3:1 to 3:17

**Unsupported Node**
at 4:1 to 4:17

**Unsupported Node**
at 6:5 to 6:7

**Unsupported Node**
at 8:14 to 8:18

**Unsupported Node**
at 9:5 to 9:11

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
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
