# META
~~~ini
description=Record update syntax
type=expr
~~~
# SOURCE
~~~roc
{ ..person, age: 31, active: True }
~~~
# EXPECTED
UNDEFINED VARIABLE - record_extension_update.md:1:5:1:11
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_extension_update.md:1:5:1:11:**
```roc
{ ..person, age: 31, active: True }
```
    ^^^^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),DoubleDot(1:3-1:5),LowerIdent(1:5-1:11),Comma(1:11-1:12),LowerIdent(1:13-1:16),OpColon(1:16-1:17),Int(1:18-1:20),Comma(1:20-1:21),LowerIdent(1:22-1:28),OpColon(1:28-1:29),UpperIdent(1:30-1:34),CloseCurly(1:35-1:36),EndOfFile(1:36-1:36),
~~~
# PARSE
~~~clojure
(e-record @1.1-1.36
	(ext
		(e-ident @1.5-1.11 (raw "person")))
	(field (field "age")
		(e-int @1.18-1.20 (raw "31")))
	(field (field "active")
		(e-tag @1.30-1.34 (raw "True"))))
~~~
# FORMATTED
~~~roc
{..person, age: 31, active: True}
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-1.36
	(ext
		(e-runtime-error (tag "ident_not_in_scope")))
	(fields
		(field (name "age")
			(e-int @1.18-1.20 (value "31")))
		(field (name "active")
			(e-tag @1.30-1.34 (name "True")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.36 (type "{ age: Num(a), active: [True]b }"))
~~~
