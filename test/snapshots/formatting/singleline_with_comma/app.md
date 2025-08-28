# META
~~~ini
description=Singleline with comma formatting app
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [a1!, a2!,], a: "a", }
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang Comma LowerIdent OpBang Comma CloseSquare Comma LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
app
{
	pf: "../basic-cli/main.roc" platform [a1, a2],
	a: (
		"a",
	),
}

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:65 to 1:65

**Expected Close Curly Brace**
at 1:1 to 1:66

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
# No top-level expression found in file
~~~
