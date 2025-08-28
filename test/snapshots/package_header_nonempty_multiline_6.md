# META
~~~ini
description=package_header_nonempty_multiline (6)
type=file
~~~
# SOURCE
~~~roc
package # Comment after keyword
	[ # Comment after exposes open
		something, # Comment after exposed item
		SomeType, # Comment after last exposed item
	]
	{ # Comment after packages open
		somePkg: "../main.roc", # Comment after package
		other: "../../other/main.roc", # Comment after last package
	}
~~~
# TOKENS
~~~text
KwPackage OpenSquare LowerIdent Comma UpperIdent Comma CloseSquare OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
package [
# Comment after exposes open
	something,
# Comment after exposed item
	SomeType,
] packages { # Comment after packages open
somePkg, (("../main.roc", # Comment after package
other) : "../../other/main.roc")}

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 9:2 to 9:2

**Expected Close Curly Brace**
at 1:1 to 9:3

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
