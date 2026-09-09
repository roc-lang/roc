# META
~~~ini
description=Malformed record syntax using equals instead of colon (error case)
type=expr
~~~
# SOURCE
~~~roc
{ age: 42, name = "Alice" }
~~~
# EXPECTED
UNEXPECTED TYPE SYNTAX - error_malformed_syntax_2.md:1:8:1:10
UNEXPECTED EXPRESSION SYNTAX - error_malformed_syntax_2.md:1:10:1:11
DECLARATION HAS NO VALUE - error_malformed_syntax_2.md:1:3:1:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Type Syntax")
		(region (start 1 8) (end 1 10))
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
			(annotated code "42")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "error_malformed_syntax_2.md") (start 1 8) (end 1 10) (annotation error) (line-text "{ age: 42, name = \"Alice\" }"))))
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
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "error_malformed_syntax_2.md") (start 1 10) (end 1 11) (annotation error) (line-text "{ age: 42, name = \"Alice\" }"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 1 3) (end 1 10))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "error_malformed_syntax_2.md") (start 1 3) (end 1 10) (annotation error) (line-text "{ age: 42, name = \"Alice\" }"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpAssign,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-type-anno (name "age")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))
		(s-decl
			(p-ident (raw "name"))
			(e-string
				(e-string-part (raw "Alice"))))))
~~~
# FORMATTED
~~~roc
{
	age :
		name = "Alice"
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "age"))
		(e-anno-only))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "name"))
		(e-string
			(e-literal (string "Alice"))))
	(e-empty_record))
~~~
# TYPES
~~~clojure
(expr (type "{}"))
~~~
