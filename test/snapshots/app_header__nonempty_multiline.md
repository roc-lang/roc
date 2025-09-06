# META
~~~ini
description=App Header = nonempty multiline
type=header
~~~
# SOURCE
~~~roc
app # This comment is here
	[main!]
	{ pf: platform "../main.roc", somePkg: "../main.roc" }
~~~
# TOKENS
~~~text
KwApp LineComment OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String Comma LowerIdent OpColon String CloseCurly ~~~
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
      (lc "somePkg")
      (str_literal_big "../main.roc")
    )
))
~~~
# FORMATTED
~~~roc
app [
	# This comment is here
	main!,
] { pf: "../main.roc" platform [], somePkg: "../main.roc" }
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
