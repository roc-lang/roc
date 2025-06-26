# META
~~~ini
description=Record construction using shorthand field syntax
type=expr
~~~
# SOURCE
~~~roc
{ name, age, email, active }
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `name` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `age` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `email` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `active` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:7),Comma(1:7-1:8),LowerIdent(1:9-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:19),Comma(1:19-1:20),LowerIdent(1:21-1:27),CloseCurly(1:28-1:29),EndOfFile(1:29-1:29),
~~~
# PARSE
~~~clojure
(e-record @1-1-1-29
	(field (field "name") (optional false))
	(field (field "age") (optional false))
	(field (field "email") (optional false))
	(field (field "active") (optional false)))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record @1-1-1-29 (ext-var 84) (id 85)
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
(expr (id 85) (type "{ name: Error, age: Error, email: Error, active: Error }"))
~~~