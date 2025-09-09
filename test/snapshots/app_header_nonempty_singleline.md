# META
~~~ini
description=App Header - nonempty singleline
type=header
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../main.roc", other: "../../other/main.roc" }
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String Comma LowerIdent OpColon String CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../main.roc")
        (block)
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
app [main!] { pf: "../main.roc" platform [], other: "../../other/main.roc" }
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
; Total type variables: 0
~~~
# TYPES
~~~roc
~~~
