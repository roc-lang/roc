# META
~~~ini
description=Record with reserved keyword fields (error case)
type=expr
~~~
# SOURCE
~~~roc
{
    if: "conditional",
    when: "pattern match",
    expect: "test assertion",
    import: "mod load",
    and: Bool.true,
    or: Bool.false,
}
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_reserved_error.md:2:7:2:8
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_reserved_error.md:2:22:2:23
UNEXPECTED TYPE SYNTAX - record_different_fields_reserved_error.md:3:11:3:12
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_reserved_error.md:3:12:3:25
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_reserved_error.md:3:25:3:26
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_reserved_error.md:3:26:3:27
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_reserved_error.md:4:11:4:12
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_reserved_error.md:4:29:4:30
IMPORT MUST BE TOP LEVEL - record_different_fields_reserved_error.md:5:5:5:11
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_reserved_error.md:5:11:5:12
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_reserved_error.md:5:23:5:24
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_reserved_error.md:6:5:6:8
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_reserved_error.md:6:19:6:20
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_reserved_error.md:7:5:7:7
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_reserved_error.md:7:19:7:20
DECLARATION HAS NO VALUE - record_different_fields_reserved_error.md:3:5:3:12
MISSING METHOD - record_different_fields_reserved_error.md:4:13:4:29
MISSING METHOD - record_different_fields_reserved_error.md:5:13:5:23
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 2 7) (end 2 8))
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
			(annotated code ":")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_different_fields_reserved_error.md") (start 2 7) (end 2 8) (annotation error) (line-text "    if: \"conditional\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 2 22) (end 2 23))
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
			(source-region (file "record_different_fields_reserved_error.md") (start 2 22) (end 2 23) (annotation error) (line-text "    if: \"conditional\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Type Syntax")
		(region (start 3 11) (end 3 12))
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
			(annotated code "\"")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_different_fields_reserved_error.md") (start 3 11) (end 3 12) (annotation error) (line-text "    when: \"pattern match\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 3 12) (end 3 25))
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
			(annotated code "pattern match")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_different_fields_reserved_error.md") (start 3 12) (end 3 25) (annotation error) (line-text "    when: \"pattern match\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 3 25) (end 3 26))
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
			(annotated code "\"")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_different_fields_reserved_error.md") (start 3 25) (end 3 26) (annotation error) (line-text "    when: \"pattern match\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 3 26) (end 3 27))
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
			(source-region (file "record_different_fields_reserved_error.md") (start 3 26) (end 3 27) (annotation error) (line-text "    when: \"pattern match\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 4 11) (end 4 12))
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
			(annotated code ":")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_different_fields_reserved_error.md") (start 4 11) (end 4 12) (annotation error) (line-text "    expect: \"test assertion\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 4 29) (end 4 30))
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
			(source-region (file "record_different_fields_reserved_error.md") (start 4 29) (end 4 30) (annotation error) (line-text "    expect: \"test assertion\","))))
	(report
		(severity runtime_error)
		(title "Import Must Be Top Level")
		(region (start 5 5) (end 5 11))
		(headline
			(reflow "I was parsing an import, but imports are only allowed at the top level."))
		(document
			(reflow "Move this import after the mod header and before declarations or executable statements.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "import Json")
			(line-break)
			(line-break)
			(indent 1)
			(text "main = 1")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "import")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "record_different_fields_reserved_error.md") (start 5 5) (end 5 11) (annotation error) (line-text "    import: \"mod load\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 5 11) (end 5 12))
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
			(annotated code ":")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_different_fields_reserved_error.md") (start 5 11) (end 5 12) (annotation error) (line-text "    import: \"mod load\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 5 23) (end 5 24))
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
			(source-region (file "record_different_fields_reserved_error.md") (start 5 23) (end 5 24) (annotation error) (line-text "    import: \"mod load\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 6 5) (end 6 8))
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
			(annotated code "and")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_different_fields_reserved_error.md") (start 6 5) (end 6 8) (annotation error) (line-text "    and: Bool.true,"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 6 19) (end 6 20))
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
			(source-region (file "record_different_fields_reserved_error.md") (start 6 19) (end 6 20) (annotation error) (line-text "    and: Bool.true,"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 7 5) (end 7 7))
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
			(annotated code "or")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_different_fields_reserved_error.md") (start 7 5) (end 7 7) (annotation error) (line-text "    or: Bool.false,"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 7 19) (end 7 20))
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
			(source-region (file "record_different_fields_reserved_error.md") (start 7 19) (end 7 20) (annotation error) (line-text "    or: Bool.false,"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 3 5) (end 3 12))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "record_different_fields_reserved_error.md") (start 3 5) (end 3 12) (annotation error) (line-text "    when: \"pattern match\","))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 4 13) (end 4 29))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "record_different_fields_reserved_error.md") (start 4 13) (end 4 29) (annotation error) (line-text "    expect: \"test assertion\","))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_quote")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{}")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 5 13) (end 5 23))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "record_different_fields_reserved_error.md") (start 5 13) (end 5 23) (annotation error) (line-text "    import: \"mod load\","))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_quote")
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
OpenCurly,
KwIf,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
KwExpect,OpColon,StringStart,StringPart,StringEnd,Comma,
KwImport,OpColon,StringStart,StringPart,StringEnd,Comma,
OpAnd,OpColon,UpperIdent,NoSpaceDotLowerIdent,Comma,
OpOr,OpColon,UpperIdent,NoSpaceDotLowerIdent,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(e-if-without-else
			(e-malformed (reason "expr_unexpected_token"))
			(e-string
				(e-string-part (raw "conditional"))))
		(e-malformed (reason "expr_unexpected_token"))
		(s-type-anno (name "when")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(s-expect
			(e-malformed (reason "expr_unexpected_token")))
		(e-string
			(e-string-part (raw "test assertion")))
		(e-malformed (reason "expr_unexpected_token"))
		(s-malformed (tag "import_must_be_top_level"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-string
			(e-string-part (raw "mod load")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-ident (raw "Bool.true"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-ident (raw "Bool.false"))
		(e-malformed (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
{
	if  "conditional"
	
	when :
			
	expect
	"test assertion"
	
			"mod load"
	
		Bool.true
	
		Bool.false
	
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-expr
		(e-runtime-error (tag "if_condition_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "when"))
		(e-anno-only))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expect
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "nested_value_not_found")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "nested_value_not_found")))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
