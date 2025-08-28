# META
~~~ini
description=Multiline formatting app
type=file
~~~
# SOURCE
~~~roc
app [
	a1!,
	a2!,
] {
	pf: platform "../basic-cli/main.roc",
	a: "a",
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang Comma LowerIdent OpBang Comma CloseSquare OpenCurly LowerIdent OpColon KwPlatform String Comma LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
app  [a1, a2] { pf: (
	(
		"../basic-cli/main.roc",
		a,
	) : "a",
) platform [] }

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 7:1 to 7:1

**Expected Close Curly Brace**
at 1:1 to 7:2

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
