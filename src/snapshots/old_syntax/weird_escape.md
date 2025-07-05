# META
~~~ini
description=weird_escape fail
type=expr
~~~
# SOURCE
~~~roc
"abc\qdef"
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**INVALID ESCAPE SEQUENCE**
This escape sequence is not recognized.

# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:10),StringEnd(1:10-1:11),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.11
	(e-string-part @1.2-1.10 (raw "abc\qdef")))
~~~
# FORMATTED
~~~roc
"abc\qdef"
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.11
	(e-literal @1.2-1.10 (string "abc\qdef")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.11 (type "Str"))
~~~
