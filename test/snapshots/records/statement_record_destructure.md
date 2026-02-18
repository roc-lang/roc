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
OpenCurly,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseCurly,OpAssign,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-record
				(field (name "name") (rest false))
				(field (name "age") (rest false))
				(field (name "email") (rest false)))
			(e-ident (raw "person")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-record-destructure
			(destructs
				(record-destruct (label "name") (ident "name")
					(required
						(p-assign (ident "name"))))
				(record-destruct (label "age") (ident "age")
					(required
						(p-assign (ident "age"))))
				(record-destruct (label "email") (ident "email")
					(required
						(p-assign (ident "email"))))))
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions
		(expr (type "Error"))))
~~~
