# META
~~~ini
description=deprecated_interpolated_string fail
type=expr
~~~
# SOURCE
~~~roc
"\(e)"
~~~
# EXPECTED
NIL
# PROBLEMS
**INVALID ESCAPE SEQUENCE**
This escape sequence is not recognized.

# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:6),StringEnd(1:6-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.7
	(e-string-part @1.2-1.6 (raw "\(e)")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.7
	(e-literal @1.2-1.6 (string "\(e)")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.7 (type "Str"))
~~~
