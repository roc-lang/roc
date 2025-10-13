# META
~~~ini
description=Debug expression stmt
type=statement
~~~
# SOURCE
~~~roc
return Bool.True
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwReturn(1:1-1:7),UpperIdent(1:8-1:12),NoSpaceDotUpperIdent(1:12-1:17),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(s-return @1.1-1.17
	(e-tag @1.8-1.17 (raw "Bool.True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-return @1.1-1.17
		(e-nominal-external @1.8-1.17
			(module-idx "2")
			(target-node-idx "1")
			(e-tag @1.8-1.17 (name "True")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
