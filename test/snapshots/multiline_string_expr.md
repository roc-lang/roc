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
MultilineStringStart,StringPart,
MultilineStringStart,StringPart,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-multiline-string
	(e-string-part (raw "This is a string"))
	(e-string-part (raw "With multiple lines")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string
	(e-literal (string "This is a string"))
	(e-literal (string "\n"))
	(e-literal (string "With multiple lines")))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
