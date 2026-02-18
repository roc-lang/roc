# META
~~~ini
description=Simple string literal
type=expr
~~~
# SOURCE
~~~roc
"hello world"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-string
	(e-string-part (raw "hello world")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string
	(e-literal (string "hello world")))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
