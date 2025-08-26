# META
~~~ini
description=package_header_nonempty_singleline (1)
type=file
~~~
# SOURCE
~~~roc
package [something, SomeType] { somePkg: "../main.roc", other: "../../other/main.roc" }
~~~
# TOKENS
~~~text
KwPackage OpenSquare LowerIdent Comma UpperIdent CloseSquare OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String CloseCurly ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
package [
	something,
	SomeType,
] packages {somePkg, ("../main.roc", other) : "../../other/main.roc"}

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
