# META
~~~ini
description=App Header - nonempty multiline - heavily commented
type=header
~~~
# SOURCE
~~~roc
app # Comment after keyword
	{ # Comment after packages open
		pf: "../main.roc" platform [ # Comment after provides open
			main!, # Comment after exposed item
		], # Comment after platform
		other: "../../other/main.roc", # Comment after last package
	}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang Comma CloseSquare Comma LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
app # Comment after keyword
{
# Comment after packages open
	pf: "../main.roc" platform [ # Comment after provides open
		main
	],
# Comment after platform
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
~~~
