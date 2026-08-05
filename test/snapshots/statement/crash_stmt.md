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
	(s-expr
		(e-run-low-level (op "crash")
			(args
				(e-string
					(e-literal (string "some message")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
