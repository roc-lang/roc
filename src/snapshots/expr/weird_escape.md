# META
~~~ini
description=Weird escape (should error)
type=expr
~~~
# SOURCE
~~~roc
"abc\qdef"
~~~
# EXPECTED
NIL
# PROBLEMS
**INVALID ESCAPE SEQUENCE**
This escape sequence is not recognized.

# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:10),StringEnd(1:10-1:11),EndOfFile(1:11-1:11),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.11
	(e-string-part @1.2-1.10 (raw "abc\qdef")))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
