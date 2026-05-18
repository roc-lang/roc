# META
~~~ini
description=Simple string interpolation
type=expr
~~~
# SOURCE
~~~roc
"Hello ${name}!"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-string
	(e-string-part (raw "Hello "))
	(e-ident (raw "name"))
	(e-string-part (raw "!")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string
	(e-literal (string "Hello "))
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-literal (string "!")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
