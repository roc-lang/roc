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
(header-only)
~~~
# FORMATTED
~~~roc
app
{
	pf: "../main.roc" platform [
		main,
	],
	other: "../../other/main.roc",
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
main : Str
~~~
