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
NAME NOT IN SCOPE - statement_record_destructure.md:1:24:1:30
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 1 24) (end 1 30))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "person")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "statement_record_destructure.md") (start 1 24) (end 1 30) (annotation error) (line-text "{ name, age, email } = person")))))
~~~
# TOKENS
~~~zig
OpenCurly,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseCurly,OpAssign,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
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
