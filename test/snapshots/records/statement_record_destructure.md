# META
~~~ini
description=Record destructuring in assignment statement
type=snippet
~~~
# SOURCE
~~~roc
{ name, age, email } = person
~~~
# EXPECTED
UNDEFINED VARIABLE - statement_record_destructure.md:1:24:1:30
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**statement_record_destructure.md:1:24:1:30:**
```roc
{ name, age, email } = person
```
                       ^^^^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:7),Comma(1:7-1:8),LowerIdent(1:9-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:19),CloseCurly(1:20-1:21),OpAssign(1:22-1:23),LowerIdent(1:24-1:30),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.30
	(type-module @1.1-1.2)
	(statements
		(s-decl @1.1-1.30
			(p-record @1.1-1.21
				(field @1.3-1.7 (name "name") (rest false))
				(field @1.9-1.12 (name "age") (rest false))
				(field @1.14-1.19 (name "email") (rest false)))
			(e-ident @1.24-1.30 (raw "person")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-record-destructure @1.1-1.21
			(destructs
				(record-destruct @1.3-1.7 (label "name") (ident "name")
					(required
						(p-assign @1.3-1.7 (ident "name"))))
				(record-destruct @1.9-1.12 (label "age") (ident "age")
					(required
						(p-assign @1.9-1.12 (ident "age"))))
				(record-destruct @1.14-1.19 (label "email") (ident "email")
					(required
						(p-assign @1.14-1.19 (ident "email"))))))
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions
		(expr @1.24-1.30 (type "Error"))))
~~~
