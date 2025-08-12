# META
~~~ini
description=Debug expression stmt
type=statement
~~~
# SOURCE
~~~roc
crash "some message"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwCrash(1:1-1:6),StringStart(1:7-1:8),StringPart(1:8-1:20),StringEnd(1:20-1:21),EndOfFile(1:21-1:21),
~~~
# PARSE
~~~clojure
(s-crash @1.1-1.21
	(e-string @1.7-1.21
		(e-string-part @1.8-1.20 (raw "some message"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-crash @1.1-1.21 (msg "some message")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
