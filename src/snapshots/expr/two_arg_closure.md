# META
~~~ini
description=two_arg_closure
type=expr
~~~
# SOURCE
~~~roc
|_, _| 42
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar(1:1-1:2),Underscore(1:2-1:3),Comma(1:3-1:4),Underscore(1:5-1:6),OpBar(1:6-1:7),Int(1:8-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-lambda @1.1-1.10
	(args
		(p-underscore)
		(p-underscore))
	(e-int @1.8-1.10 (raw "42")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda @1.1-1.10
	(args
		(p-underscore @1.2-1.3)
		(p-underscore @1.5-1.6))
	(e-int @1.8-1.10 (value "42")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.10 (type "a, b -> Num(c)"))
~~~
