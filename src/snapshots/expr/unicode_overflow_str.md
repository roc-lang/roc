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
StringStart(1:1-1:2),StringPart(1:2-1:12),StringEnd(1:12-1:13),EndOfFile(1:13-1:13),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.13
	(e-string-part @1.2-1.12 (raw "\u(FFFFFF)")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-str @1.1-1.13
	(e-literal @1.2-1.12 (string "\u(FFFFFF)")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.13 (type "Str"))
~~~
