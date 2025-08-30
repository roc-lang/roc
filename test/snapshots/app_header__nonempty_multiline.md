# META
~~~ini
description=App Header = nonempty multiline
type=header
~~~
# SOURCE
~~~roc
app # This comment is here
	{ pf: "../main.roc" platform [main!], somePkg: "../main.roc" }
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
          (lc "main")
        )
      )
    )

    (binop_colon
      (lc "somePkg")
      (str_literal_big "../main.roc")
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../main.roc" platform [main], somePkg: "../main.roc" }

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
