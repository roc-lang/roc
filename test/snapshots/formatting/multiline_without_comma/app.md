# META
~~~ini
description=Multiline without comma formatting app
type=file
~~~
# SOURCE
~~~roc
app [
	a1!,
	a2!
] {
	pf: platform "../basic-cli/main.roc",
	a: "a"
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String Comma LowerIdent OpColon String CloseCurly ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
app  [a1, a2]
{
	pf: ("../basic-cli/main.roc", a) : "a" platform [],
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
# No top-level expression found in file
~~~
