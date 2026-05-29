# META
~~~ini
description=Record with field update syntax
type=expr
~~~
# SOURCE
~~~roc
{ ..person, age: 31 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(ext
		(e-ident (raw "person")))
	(field (field "age")
		(e-int (raw "31"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record
	(ext
		(e-runtime-error (tag "ident_not_in_scope")))
	(fields
		(field (name "age")
			(e-num (value "31")))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
