# META
~~~ini
description=App Header - platform not specified first
type=header
~~~
# SOURCE
~~~roc
app
	[main!,]
	{ somePkg: "../main.roc", pf: platform "../main.roc", }
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang Comma CloseSquare OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon KwPlatform String Comma CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "somePkg")
      (str_literal_big "../main.roc")
    )

    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../main.roc")
        (block)
      )
    )
))
~~~
# FORMATTED
~~~roc
app [
	main!,
]
{
	somePkg: "../main.roc",
	pf: "../main.roc" platform [],
}
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
~~~
# TYPES
~~~roc
~~~
