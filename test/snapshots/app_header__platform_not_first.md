# META
~~~ini
description=App Header - platform not specified first
type=header
~~~
# SOURCE
~~~roc
app
	{ somePkg: "../main.roc", pf: "../main.roc" platform [main!,], }
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang Comma CloseSquare Comma CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "somePkg")
      (str_literal_big "../main.roc")
    )

    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../main.roc")
        (block
          (lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app
{
	somePkg: "../main.roc",
	pf: "../main.roc" platform [main],
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
; No expression to type check
~~~
# TYPES
~~~roc
main : _a
~~~
