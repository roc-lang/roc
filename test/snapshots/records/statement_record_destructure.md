# META
~~~ini
description=Record destructuring in assignment statement
type=file:StatementRecordDestructure.roc
~~~
# SOURCE
~~~roc
StatementRecordDestructure := {}

{ name, age, email } = person
~~~
# EXPECTED
UNDEFINED VARIABLE - statement_record_destructure.md:3:24:3:30
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**statement_record_destructure.md:3:24:3:30:**
```roc
{ name, age, email } = person
```
                       ^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:27),OpColonEqual(1:28-1:30),OpenCurly(1:31-1:32),CloseCurly(1:32-1:33),
OpenCurly(3:1-3:2),LowerIdent(3:3-3:7),Comma(3:7-3:8),LowerIdent(3:9-3:12),Comma(3:12-3:13),LowerIdent(3:14-3:19),CloseCurly(3:20-3:21),OpAssign(3:22-3:23),LowerIdent(3:24-3:30),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.30
	(type-module @1.1-1.27)
	(statements
		(s-type-decl @1.1-1.33
			(header @1.1-1.27 (name "StatementRecordDestructure")
				(args))
			(ty-record @1.31-1.33))
		(s-decl @3.1-3.30
			(p-record @3.1-3.21
				(field @3.3-3.7 (name "name") (rest false))
				(field @3.9-3.12 (name "age") (rest false))
				(field @3.14-3.19 (name "email") (rest false)))
			(e-ident @3.24-3.30 (raw "person")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-record-destructure @3.1-3.21
			(destructs
				(record-destruct @3.3-3.7 (label "name") (ident "name")
					(required
						(p-assign @3.3-3.7 (ident "name"))))
				(record-destruct @3.9-3.12 (label "age") (ident "age")
					(required
						(p-assign @3.9-3.12 (ident "age"))))
				(record-destruct @3.14-3.19 (label "email") (ident "email")
					(required
						(p-assign @3.14-3.19 (ident "email"))))))
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-nominal-decl @1.1-1.33
		(ty-header @1.1-1.27 (name "StatementRecordDestructure"))
		(ty-record @1.31-1.33)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.33 (type "StatementRecordDestructure")
			(ty-header @1.1-1.27 (name "StatementRecordDestructure"))))
	(expressions
		(expr @3.24-3.30 (type "Error"))))
~~~
