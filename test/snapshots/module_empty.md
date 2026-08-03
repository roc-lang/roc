# META
~~~ini
description=An empty mod with no exposes
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
	(type-mod)
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
