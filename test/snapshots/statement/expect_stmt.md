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
KwExpect,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-expect
	(e-tag (raw "Bool.True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-expect
		(e-nominal-external
			(builtin)
			(e-tag (name "True")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
