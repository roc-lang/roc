# META
~~~ini
description=An empty module with multiline exposes and comments
type=file
~~~
# SOURCE
~~~roc
module # Comment after module keyword
	[ # Comment After exposes open
		something, # Comment after exposed item
		SomeType, # Comment after final exposed item
	]
~~~
# TOKENS
~~~text
KwModule LineComment OpenSquare LineComment LowerIdent Comma LineComment UpperIdent Comma LineComment CloseSquare ~~~
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

# Comment after module keyword
# Comment After exposes open
# Comment after exposed item
# Comment after final exposed item
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
