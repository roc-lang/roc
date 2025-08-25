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
(header-only)
~~~
# FORMATTED
~~~roc
app { pf: ("../main.roc" platform [main]), somePkg: "../main.roc" }

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
