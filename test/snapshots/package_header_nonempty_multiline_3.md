# META
~~~ini
description=package_header_nonempty_multiline (3)
type=file
~~~
# SOURCE
~~~roc
package
	[something, SomeType,]
	{ somePkg: "../main.roc", }
~~~
# TOKENS
~~~text
KwPackage OpenSquare LowerIdent Comma UpperIdent Comma CloseSquare OpenCurly LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(package-header
  (exposes
    (lc "something")

    (uc "SomeType")
)
  (packages
    (lc "somePkg")

    (str_literal_big "../main.roc")
))
~~~
# FORMATTED
~~~roc
package [
	something,
	SomeType,
] packages {somePkg, "../main.roc"}
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_3.md:2:3:2:12
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_3.md:2:14:2:22
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
; Total type variables: 0
~~~
# TYPES
~~~roc
~~~
