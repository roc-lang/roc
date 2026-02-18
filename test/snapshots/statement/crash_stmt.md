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
KwCrash,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-crash
	(e-string
		(e-string-part (raw "some message"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-crash (msg "some message")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
