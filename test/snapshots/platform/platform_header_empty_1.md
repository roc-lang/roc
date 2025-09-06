# META
~~~ini
description=platform_header_empty (1)
type=file
~~~
# SOURCE
~~~roc
platform "foo"
	requires {} {}
	exposes []
	packages {}
	provides []
~~~
# TOKENS
~~~text
KwPlatform String KwRequires OpenCurly CloseCurly OpenCurly CloseCurly KwExposes OpenSquare CloseSquare KwPackages OpenCurly CloseCurly KwProvides OpenSquare CloseSquare ~~~
# PARSE
~~~clojure
(platform-header)
~~~
# FORMATTED
~~~roc
platform "foo" requires {} exposes []
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
~~~
# TYPES
~~~roc
# Header type not yet fully supported
~~~
