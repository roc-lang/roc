# META
~~~ini
description=package_header_nonempty_multiline (1)
type=file
~~~
# SOURCE
~~~roc
package # This comment is here
	[something, SomeType]
	{ somePkg: "../main.roc" }
~~~
# TOKENS
~~~text
KwPackage OpenSquare LowerIdent Comma UpperIdent CloseSquare OpenCurly LowerIdent OpColon String CloseCurly ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_1.md:2:3:2:12
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_1.md:2:14:2:22
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
