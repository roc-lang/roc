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
KwReturn,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-return
	(e-tag (raw "Bool.True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-return
		(e-nominal-external
			(external-module "Bool")
			(e-tag (name "True")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
