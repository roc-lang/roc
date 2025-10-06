# META
~~~ini
description=An empty module with singleline exposes
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
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @2.1-2.1
	(type-module @2.1-2.1)
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
