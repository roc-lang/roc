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
  (binop_equals
    (not_lc "main")
    (lambda
      (body
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
          (apply_anon
            (binop_pipe
              (uc "Stdout")
              (not_lc "line")
            )
            (lc "result")
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
app
{
	pf: "../basic-cli/platform.roc" platform [
		main,
	],
}

import pf.Stdout
import json.Json

main! = \_ -> {
	result = Json.utf8("Hello from external module!")
	Stdout.line!(result)
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
