# META
~~~ini
description=Type annotation missing parentheses for type application
type=snippet
~~~
# SOURCE
~~~roc
nums : List U8
~~~
# EXPECTED
TYPE APPLICATION NEEDS PARENTHESES - type_annotation_missing_parens.md:2:1:2:1
TOO FEW ARGS - type_annotation_missing_parens.md:1:8:1:12
DECLARATION HAS NO VALUE - type_annotation_missing_parens.md:1:1:1:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 2 1) (end 2 1))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "type_annotation_missing_parens.md") (start 2 1) (end 2 1) (annotation error) (line-text ""))))
	(report
		(severity runtime_error)
		(title "Too Few Args")
		(region (start 1 8) (end 1 12))
		(headline
			(reflow "The type")
			(reflow " ")
			(annotated type "List")
			(reflow " ")
			(reflow "expects")
			(reflow " ")
			(reflow "1")
			(reflow " ")
			(reflow "argument,")
			(reflow " ")
			(reflow "but got")
			(reflow " ")
			(reflow "0")
			(reflow " ")
			(reflow "instead."))
		(document
			(source-region (file "type_annotation_missing_parens.md") (start 1 8) (end 1 12) (annotation error) (line-text "nums : List U8"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 1 1) (end 1 12))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "type_annotation_missing_parens.md") (start 1 1) (end 1 12) (annotation error) (line-text "nums : List U8"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "nums")
			(ty (name "List")))
		(s-malformed (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
nums : List
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "nums"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "List") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
