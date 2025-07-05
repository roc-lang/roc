# META
~~~ini
description=Record with field update syntax
type=expr
~~~
# SOURCE
~~~roc
{ ..person, age: 31 }
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),DoubleDot(1:3-1:5),LowerIdent(1:5-1:11),Comma(1:11-1:12),LowerIdent(1:13-1:16),OpColon(1:16-1:17),Int(1:18-1:20),CloseCurly(1:21-1:22),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-record @1.1-1.22
	(ext
		(e-ident @1.5-1.11 (raw "person")))
	(field (field "age") (optional false)
		(e-int @1.18-1.20 (raw "31"))))
~~~
# FORMATTED
~~~roc
{..person, age: 31}
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-1.22
	(ext
		(e-runtime-error (tag "ident_not_in_scope")))
	(fields
		(field (name "age")
			(e-int @1.18-1.20 (value "31")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.22 (type "{ age: Num(*) }"))
~~~
