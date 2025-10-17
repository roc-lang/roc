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
OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(ext
		(e-ident (raw "person")))
	(field (field "age")
		(e-int (raw "31")))
	(field (field "active")
		(e-tag (raw "True"))))
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
			(e-num (value "31")))
		(field (name "active")
			(e-nominal (nominal "Bool")
				(e-tag (name "True"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ active: Bool, age: Num(_size), Error }"))
~~~
