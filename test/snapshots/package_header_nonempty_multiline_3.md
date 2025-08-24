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
(header-only)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_3.md:2:3:2:12
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_3.md:2:14:2:22
# PROBLEMS
**Parse Error**
at 3:28 to 3:28

**Expected Close Curly Brace**
at 1:1 to 3:29

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
