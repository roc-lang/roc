# META
~~~ini
description=App Header - nonempty multiline w/ trailing comma
type=header
~~~
# SOURCE
~~~roc
app
	[main!,]
	{ pf: platform "../main.roc", somePkg: "../main.roc", }
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang Comma CloseSquare OpenCurly LowerIdent OpColon KwPlatform String Comma LowerIdent OpColon String Comma CloseCurly ~~~
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
	main!,
]
{
	pf: "../main.roc" platform [],
	somePkg: "../main.roc",
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
