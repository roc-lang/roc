# META
~~~ini
description=Singleline formatting app
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [a1!, a2!], a: "a" }
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare Comma LowerIdent OpColon String CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (not_lc "a1")
          (not_lc "a2")
        )
      )
    )

    (binop_colon
      (lc "a")
      (str_literal_small "a")
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [a1!, a2!], a: "a" }
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
# No top-level expression found in file
~~~
