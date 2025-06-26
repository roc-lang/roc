# META
~~~ini
description=Record destructuring in let binding statement
type=statement
~~~
# SOURCE
~~~roc
{ name, age, email } = person
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:7),Comma(1:7-1:8),LowerIdent(1:9-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:19),CloseCurly(1:20-1:21),OpAssign(1:22-1:23),LowerIdent(1:24-1:30),EndOfFile(1:30-1:30),
~~~
# PARSE
~~~clojure
(e-record @1-1-1-21
	(field (field "name") (optional false))
	(field (field "age") (optional false))
	(field (field "email") (optional false)))
~~~
# FORMATTED
~~~roc
{ name, age, email }
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~