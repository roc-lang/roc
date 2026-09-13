# META
~~~ini
description=record_builder
type=expr
~~~
# SOURCE
~~~roc
{ Foo.Bar.baz <-
    x: 5,
    y: 0,
}
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - record_builder.md:1:15:1:17
UNEXPECTED TYPE SYNTAX - record_builder.md:2:8:2:9
UNEXPECTED EXPRESSION SYNTAX - record_builder.md:2:9:2:10
UNEXPECTED TYPE SYNTAX - record_builder.md:3:8:3:9
UNEXPECTED EXPRESSION SYNTAX - record_builder.md:3:9:3:10
DECLARATION HAS NO VALUE - record_builder.md:2:5:2:9
DECLARATION HAS NO VALUE - record_builder.md:3:5:3:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 1 15) (end 1 17))
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
			(annotated code "<-")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_builder.md") (start 1 15) (end 1 17) (annotation error) (line-text "{ Foo.Bar.baz <-"))))
	(report
		(severity runtime_error)
		(title "Unexpected Type Syntax")
		(region (start 2 8) (end 2 9))
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
			(annotated code "5")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_builder.md") (start 2 8) (end 2 9) (annotation error) (line-text "    x: 5,"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 2 9) (end 2 10))
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
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "record_builder.md") (start 2 9) (end 2 10) (annotation error) (line-text "    x: 5,"))))
	(report
		(severity runtime_error)
		(title "Unexpected Type Syntax")
		(region (start 3 8) (end 3 9))
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
			(annotated code "0")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_builder.md") (start 3 8) (end 3 9) (annotation error) (line-text "    y: 0,"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 3 9) (end 3 10))
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
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "record_builder.md") (start 3 9) (end 3 10) (annotation error) (line-text "    y: 0,"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 2 5) (end 2 9))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "record_builder.md") (start 2 5) (end 2 9) (annotation error) (line-text "    x: 5,"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 3 5) (end 3 9))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "record_builder.md") (start 3 5) (end 3 9) (annotation error) (line-text "    y: 0,"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
OpenCurly,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpBackArrow,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(e-ident (raw "Foo.Bar.baz"))
		(e-malformed (reason "expr_unexpected_token"))
		(s-type-anno (name "x")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))
		(s-type-anno (name "y")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
{
	Foo.Bar.baz
	
	x :
	
	y :
	
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-expr
		(e-runtime-error (tag "qualified_ident_does_not_exist")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "x"))
		(e-anno-only))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "y"))
		(e-anno-only))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
