# META
~~~ini
description=Record construction using shorthand field syntax
type=expr
~~~
# SOURCE
~~~roc
{ name, age, email, active }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "name"))
	(field (field "age"))
	(field (field "email"))
	(field (field "active")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "name")
			(e-runtime-error (tag "ident_not_in_scope")))
		(field (name "age")
			(e-runtime-error (tag "ident_not_in_scope")))
		(field (name "email")
			(e-runtime-error (tag "ident_not_in_scope")))
		(field (name "active")
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(expr (type "{ active: Error, age: Error, email: Error, name: Error }"))
~~~
