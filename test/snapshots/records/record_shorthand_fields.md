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
UNDEFINED VARIABLE - record_shorthand_fields.md:1:3:1:7
UNDEFINED VARIABLE - record_shorthand_fields.md:1:9:1:12
UNDEFINED VARIABLE - record_shorthand_fields.md:1:14:1:19
UNDEFINED VARIABLE - record_shorthand_fields.md:1:21:1:27
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `name` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_shorthand_fields.md:1:3:1:7:**
```roc
{ name, age, email, active }
```
  ^^^^


**UNDEFINED VARIABLE**
Nothing is named `age` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_shorthand_fields.md:1:9:1:12:**
```roc
{ name, age, email, active }
```
        ^^^


**UNDEFINED VARIABLE**
Nothing is named `email` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_shorthand_fields.md:1:14:1:19:**
```roc
{ name, age, email, active }
```
             ^^^^^


**UNDEFINED VARIABLE**
Nothing is named `active` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_shorthand_fields.md:1:21:1:27:**
```roc
{ name, age, email, active }
```
                    ^^^^^^


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
