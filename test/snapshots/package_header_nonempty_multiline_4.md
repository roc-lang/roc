# META
~~~ini
description=package_header_nonempty_multiline (4)
type=file
~~~
# SOURCE
~~~roc
package
	[
		something,
		SomeType,
	]
	{
		somePkg: "../main.roc",
	}
~~~
# TOKENS
~~~text
KwPackage OpenSquare LowerIdent Comma UpperIdent Comma CloseSquare OpenCurly LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
package [
	something,
	SomeType,
] packages {somePkg, ("../main.roc")}

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 8:2 to 8:2

**Expected Close Curly Brace**
at 1:1 to 8:3

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
