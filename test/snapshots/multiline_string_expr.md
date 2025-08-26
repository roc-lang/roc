# META
~~~ini
description=multiline_string_expr
type=expr
~~~
# SOURCE
~~~roc
"""This is a string
"""With multiple lines
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
MultilineStringStart(1:1-1:4),StringPart(1:4-1:20),
MultilineStringStart(2:1-2:4),StringPart(2:4-2:23),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(e-multiline-string @1.1-2.23
	(e-string-part @1.4-1.20 (raw "This is a string"))
	(e-string-part @2.4-2.23 (raw "With multiple lines")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-2.23
	(e-literal @1.4-1.20 (string "This is a string"))
	(e-literal @2.1-2.4 (string "\n"))
	(e-literal @2.4-2.23 (string "With multiple lines")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.23 (type "Str"))
~~~
