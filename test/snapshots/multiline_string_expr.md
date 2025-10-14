# META
~~~ini
description=multiline_string_expr
type=expr
~~~
# SOURCE
~~~roc
\\This is a string
\\With multiple lines
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
MultilineStringStart(1:1-1:3),StringPart(1:3-1:19),
MultilineStringStart(2:1-2:3),StringPart(2:3-2:22),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(e-multiline-string @1.1-2.22
	(e-string-part @1.3-1.19 (raw "This is a string"))
	(e-string-part @2.3-2.22 (raw "With multiple lines")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-2.22
	(e-literal @1.3-1.19 (string "This is a string"))
	(e-literal @2.1-2.3 (string "\n"))
	(e-literal @2.3-2.22 (string "With multiple lines")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.22 (type "Str"))
~~~
