# META
~~~ini
description=dot-int tuple index access on a plain identifier
type=snippet
~~~
# SOURCE
~~~roc
foo = asd.0
~~~
# EXPECTED
NAME NOT IN SCOPE - expr_no_space_dot_int.md:1:7:1:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 1 7) (end 1 10))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "asd")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "expr_no_space_dot_int.md") (start 1 7) (end 1 10) (annotation error) (line-text "foo = asd.0")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,NoSpaceDotInt,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-tuple-access
				(e-ident (raw "asd"))
				".0"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
