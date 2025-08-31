# META
~~~ini
description=App Header - nonempty singleline
type=header
~~~
# SOURCE
~~~roc
app { pf: "../main.roc" platform [main!], other: "../../other/main.roc" }
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare Comma LowerIdent OpColon String CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../main.roc")
        (block
          (not_lc "main")
        )
      )
    )

    (binop_colon
      (lc "other")
      (str_literal_big "../../other/main.roc")
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../main.roc" platform [main!], other: "../../other/main.roc" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
main : _a
~~~
