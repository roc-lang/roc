# META
~~~ini
description=An empty module with no exposes
type=file
~~~
# SOURCE
~~~roc
module []
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.10
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
