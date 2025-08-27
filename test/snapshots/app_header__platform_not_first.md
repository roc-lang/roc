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
(header-only)
~~~
# FORMATTED
~~~roc
app
{
	somePkg: ("../main.roc", pf) : "../main.roc" platform [
		main,
	],
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
