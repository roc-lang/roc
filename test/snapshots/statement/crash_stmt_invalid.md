# META
~~~ini
description=Crash statement with invalid non-string argument
type=statement
~~~
# SOURCE
~~~roc
crash 42
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwCrash,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-crash
	(e-int (raw "42")))
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
				(e-num (value "42"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
