# META
~~~ini
description=Record destructuring in assignment statement
type=file
~~~
# SOURCE
~~~roc
module []

{ name, age, email } = person
~~~
# EXPECTED
UNDEFINED VARIABLE - statement_record_destructure.md:3:24:3:30
# PROBLEMS
**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'person' is not defined:
**statement_record_destructure.md:3:24:3:30:**
```roc
{ name, age, email } = person
```
                       ^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
OpenCurly(3:1-3:2),LowerIdent(3:3-3:7),Comma(3:7-3:8),LowerIdent(3:9-3:12),Comma(3:12-3:13),LowerIdent(3:14-3:19),CloseCurly(3:20-3:21),OpAssign(3:22-3:23),LowerIdent(3:24-3:30),EndOfFile(3:30-3:30),
~~~
# PARSE
~~~clojure
(file @1.1-3.30
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
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
	(def
		(pattern
			(p-record-destructure @3.1-3.21
				(destructs
					(record-destruct @3.3-3.7 (label "name") (ident "name")
						(required))
					(record-destruct @3.9-3.12 (label "age") (ident "age")
						(required))
					(record-destruct @3.14-3.19 (label "email") (ident "email")
						(required)))))
		(expr
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions
		(expr @1.1-1.1 (type "Error"))))
~~~
