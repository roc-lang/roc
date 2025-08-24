# META
~~~ini
description=An empty module with a singleline exposes with trailing comma
type=file
~~~
# SOURCE
~~~roc
module [something, SomeType,]
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma UpperIdent Comma CloseSquare ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - module_singleline_fmts_to_multiline.md:1:9:1:18
EXPOSED BUT NOT DEFINED - module_singleline_fmts_to_multiline.md:1:20:1:28
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
