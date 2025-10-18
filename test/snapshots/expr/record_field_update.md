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
UNDEFINED VARIABLE - record_field_update.md:1:5:1:11
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_field_update.md:1:5:1:11:**
```roc
{ ..person, age: 31 }
```
    ^^^^^^


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
(expr (type "{ age: Num(_size), Error }"))
~~~
