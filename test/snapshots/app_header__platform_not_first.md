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
(header-only)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 3:32 to 3:32

**Expected Close Curly Brace**
at 1:1 to 3:41

**No Platform**
at 1:1 to 3:41

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
