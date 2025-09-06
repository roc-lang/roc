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
(module-header
  (exposes
    (lc "something")

    (uc "SomeType")
))
~~~
# FORMATTED
~~~roc
module [
	something,
	SomeType,
]
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
~~~
# TYPES
~~~roc
~~~
