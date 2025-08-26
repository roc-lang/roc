# META
~~~ini
description=platform_header_empty (4)
type=file
~~~
# SOURCE
~~~roc
platform "foo" requires {} {} exposes [] packages {} provides []
~~~
# TOKENS
~~~text
KwPlatform String KwRequires OpenCurly CloseCurly OpenCurly CloseCurly KwExposes OpenSquare CloseSquare KwPackages OpenCurly CloseCurly KwProvides OpenSquare CloseSquare ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
platform "foo" requires  exposes  []

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:29 to 1:29

**Parse Error**
at 1:1 to 1:31

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
