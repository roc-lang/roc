# META
~~~ini
description=Unicode overflow (should error)
type=expr
~~~
# SOURCE
~~~roc
"\u(FFFFFF)"
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
	(e-string-part (raw "\u(FFFFFF)")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string
	(e-literal (string "\u(FFFFFF)")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
