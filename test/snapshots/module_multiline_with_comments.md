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
module [ # Comment after module keyword
	# Comment After exposes open
	something,
	# Comment after exposed item
	SomeType,
]# Comment after final exposed item
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - module_multiline_with_comments.md:3:3:3:12
EXPOSED BUT NOT DEFINED - module_multiline_with_comments.md:4:3:4:11
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
