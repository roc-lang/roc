# META
~~~ini
description=Debug expression stmt
type=statement
~~~
# SOURCE
~~~roc
expect Bool.True
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwExpect(1:1-1:7),UpperIdent(1:8-1:12),NoSpaceDotUpperIdent(1:12-1:17),EndOfFile(1:17-1:17),
~~~
# PARSE
~~~clojure
(s-expect @1.1-1.17
	(e-tag @1.8-1.17 (raw "Bool.True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-expect @1.1-1.17
		(e-nominal @1.8-1.12 (nominal "Bool")
			(e-tag @1.8-1.17 (name "True")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
