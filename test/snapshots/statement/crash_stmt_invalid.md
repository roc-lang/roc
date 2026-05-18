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
	(s-runtime-error (tag "crash_expects_string")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
