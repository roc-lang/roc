# META
~~~ini
description=An empty module with a singleline exposes with trailing comma
type=snippet
~~~
# SOURCE
~~~roc

~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements))
~~~
# FORMATTED
~~~roc
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
