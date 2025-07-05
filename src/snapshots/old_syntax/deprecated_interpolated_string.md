# META
~~~ini
description=deprecated_interpolated_string fail
type=expr
~~~
# SOURCE
~~~roc
"\(e)"
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**INVALID ESCAPE SEQUENCE**
This escape sequence is not recognized.

# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:6),StringEnd(1:6-1:7),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.7
	(e-string-part @1.2-1.6 (raw "\(e)")))
~~~
# FORMATTED
~~~roc
"\(e)"
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
