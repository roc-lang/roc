# META
~~~ini
description=if expression without an else branch produces a parse error
type=snippet
~~~
# SOURCE
~~~roc
foo = if tru 0
~~~
# EXPECTED
NAME NOT IN SCOPE - expr_if_missing_else.md:1:10:1:13
MISSING METHOD - expr_if_missing_else.md:1:14:1:15
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 1 10) (end 1 13))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "tru")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "expr_if_missing_else.md") (start 1 10) (end 1 13) (annotation error) (line-text "foo = if tru 0"))))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 1 14) (end 1 15))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_numeral")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "expr_if_missing_else.md") (start 1 14) (end 1 15) (annotation error) (line-text "foo = if tru 0"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_numeral")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{}")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,KwIf,LowerIdent,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-if-without-else
				(e-ident (raw "tru"))
				(e-int (raw "0"))))))
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
		(e-if
			(if-branches
				(if-branch
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-runtime-error (tag "erroneous_value_expr"))))
			(if-else
				(e-empty_record)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{}")))
	(expressions
		(expr (type "{}"))))
~~~
