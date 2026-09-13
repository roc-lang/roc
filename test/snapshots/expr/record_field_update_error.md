# META
~~~ini
description=Record with field update using old syntax (should give nice error message)
type=expr
~~~
# SOURCE
~~~roc
{ person & age: 31 }
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - record_field_update_error.md:1:10:1:11
UNEXPECTED TYPE SYNTAX - record_field_update_error.md:1:17:1:19
DECLARATION HAS NO VALUE - record_field_update_error.md:1:12:1:19
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 1 10) (end 1 11))
		(headline
			(reflow "I was parsing an expression, and this token cannot start an expression here."))
		(document
			(reflow "Expressions can be names, literals, tags, records, lists, tuples, lambdas, blocks, conditionals, matches, or function calls.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "add(1, 2)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "&")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_field_update_error.md") (start 1 10) (end 1 11) (annotation error) (line-text "{ person & age: 31 }"))))
	(report
		(severity runtime_error)
		(title "Unexpected Type Syntax")
		(region (start 1 17) (end 1 19))
		(headline
			(reflow "I was parsing a type annotation, and this token cannot start a type here."))
		(document
			(reflow "Types can be type variables, uppercase type names, function types, tuples, records, or tag unions.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "31")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_field_update_error.md") (start 1 17) (end 1 19) (annotation error) (line-text "{ person & age: 31 }"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 1 12) (end 1 19))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "record_field_update_error.md") (start 1 12) (end 1 19) (annotation error) (line-text "{ person & age: 31 }"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
OpenCurly,LowerIdent,OpAmpersand,LowerIdent,OpColon,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(e-ident (raw "person"))
		(e-malformed (reason "expr_unexpected_token"))
		(s-type-anno (name "age")
			(ty-malformed (tag "ty_anno_unexpected_token")))))
~~~
# FORMATTED
~~~roc
{
	person
		age :
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-expr
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "age"))
		(e-anno-only))
	(e-empty_record))
~~~
# TYPES
~~~clojure
(expr (type "{}"))
~~~
