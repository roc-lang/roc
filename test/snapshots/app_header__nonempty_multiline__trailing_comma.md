# META
~~~ini
description=App Header - nonempty multiline w/ trailing comma
type=header
~~~
# SOURCE
~~~roc
app
	{ pf: "../main.roc" platform [main!,], somePkg: "../main.roc", }
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
app
{
	pf: "../main.roc" platform [main],
	somePkg: ("../main.roc"),
}

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:65 to 2:65

**Expected Close Curly Brace**
at 1:1 to 2:66

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
