# META
~~~ini
description=Record with special character fields (error cases)
type=expr
~~~
# SOURCE
~~~roc
{
    _privateField: "leading underscore",
    field_: "trailing underscore",
    PascalCase: "pascal",
    kebab-case: "kebab",
    field$special: "dollar",
    field@symbol: "at symbol",
}
~~~
# EXPECTED
STRAY DOLLAR SIGN - record_different_fields_error.md:6:10:6:11
UNEXPECTED TYPE SYNTAX - record_different_fields_error.md:2:20:2:21
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:2:21:2:39
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:2:39:2:40
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:2:40:2:41
UNEXPECTED TYPE SYNTAX - record_different_fields_error.md:3:13:3:14
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:3:14:3:33
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:3:33:3:34
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:3:34:3:35
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:4:15:4:16
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:4:25:4:26
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:5:15:5:16
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:5:24:5:25
UNEXPECTED TYPE SYNTAX - record_different_fields_error.md:6:20:6:21
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:6:21:6:27
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:6:27:6:28
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:6:28:6:29
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:7:10:7:17
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:7:17:7:18
UNEXPECTED EXPRESSION SYNTAX - record_different_fields_error.md:7:30:7:31
DECLARATION HAS NO VALUE - record_different_fields_error.md:2:5:2:21
DECLARATION HAS NO VALUE - record_different_fields_error.md:3:5:3:14
TYPE MISMATCH - record_different_fields_error.md:4:5:4:15
MISSING METHOD - record_different_fields_error.md:4:17:4:25
MISSING METHOD - record_different_fields_error.md:5:17:5:24
DECLARATION HAS NO VALUE - record_different_fields_error.md:6:5:6:21
MISSING METHOD - record_different_fields_error.md:7:19:7:30
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Stray Dollar Sign")
		(region (start 6 10) (end 6 11))
		(headline
			(reflow "Dollar sign ($) is only allowed at the very beginning of a name, not in the middle or at the end."))
		(document
			(source-region (file "record_different_fields_error.md") (start 6 10) (end 6 11) (annotation error) (line-text "    field$special: \"dollar\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Type Syntax")
		(region (start 2 20) (end 2 21))
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
			(source-region (file "record_different_fields_error.md") (start 2 20) (end 2 21) (annotation error) (line-text "    _privateField: \"leading underscore\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 2 21) (end 2 39))
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
			(annotated code "leading underscore")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_different_fields_error.md") (start 2 21) (end 2 39) (annotation error) (line-text "    _privateField: \"leading underscore\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 2 39) (end 2 40))
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
			(source-region (file "record_different_fields_error.md") (start 2 39) (end 2 40) (annotation error) (line-text "    _privateField: \"leading underscore\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 2 40) (end 2 41))
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
			(source-region (file "record_different_fields_error.md") (start 2 40) (end 2 41) (annotation error) (line-text "    _privateField: \"leading underscore\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Type Syntax")
		(region (start 3 13) (end 3 14))
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
			(source-region (file "record_different_fields_error.md") (start 3 13) (end 3 14) (annotation error) (line-text "    field_: \"trailing underscore\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 3 14) (end 3 33))
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
			(annotated code "trailing underscore")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_different_fields_error.md") (start 3 14) (end 3 33) (annotation error) (line-text "    field_: \"trailing underscore\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 3 33) (end 3 34))
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
			(source-region (file "record_different_fields_error.md") (start 3 33) (end 3 34) (annotation error) (line-text "    field_: \"trailing underscore\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 3 34) (end 3 35))
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
			(source-region (file "record_different_fields_error.md") (start 3 34) (end 3 35) (annotation error) (line-text "    field_: \"trailing underscore\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 4 15) (end 4 16))
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
			(source-region (file "record_different_fields_error.md") (start 4 15) (end 4 16) (annotation error) (line-text "    PascalCase: \"pascal\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 4 25) (end 4 26))
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
			(source-region (file "record_different_fields_error.md") (start 4 25) (end 4 26) (annotation error) (line-text "    PascalCase: \"pascal\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 5 15) (end 5 16))
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
			(source-region (file "record_different_fields_error.md") (start 5 15) (end 5 16) (annotation error) (line-text "    kebab-case: \"kebab\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 5 24) (end 5 25))
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
			(source-region (file "record_different_fields_error.md") (start 5 24) (end 5 25) (annotation error) (line-text "    kebab-case: \"kebab\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Type Syntax")
		(region (start 6 20) (end 6 21))
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
			(source-region (file "record_different_fields_error.md") (start 6 20) (end 6 21) (annotation error) (line-text "    field$special: \"dollar\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 6 21) (end 6 27))
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
			(annotated code "dollar")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_different_fields_error.md") (start 6 21) (end 6 27) (annotation error) (line-text "    field$special: \"dollar\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 6 27) (end 6 28))
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
			(source-region (file "record_different_fields_error.md") (start 6 27) (end 6 28) (annotation error) (line-text "    field$special: \"dollar\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 6 28) (end 6 29))
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
			(source-region (file "record_different_fields_error.md") (start 6 28) (end 6 29) (annotation error) (line-text "    field$special: \"dollar\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 7 10) (end 7 17))
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
			(annotated code "@symbol")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_different_fields_error.md") (start 7 10) (end 7 17) (annotation error) (line-text "    field@symbol: \"at symbol\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 7 17) (end 7 18))
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
			(source-region (file "record_different_fields_error.md") (start 7 17) (end 7 18) (annotation error) (line-text "    field@symbol: \"at symbol\","))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 7 30) (end 7 31))
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
			(source-region (file "record_different_fields_error.md") (start 7 30) (end 7 31) (annotation error) (line-text "    field@symbol: \"at symbol\","))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 2 5) (end 2 21))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "record_different_fields_error.md") (start 2 5) (end 2 21) (annotation error) (line-text "    _privateField: \"leading underscore\","))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 3 5) (end 3 14))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "record_different_fields_error.md") (start 3 5) (end 3 14) (annotation error) (line-text "    field_: \"trailing underscore\","))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 4 5) (end 4 15))
		(headline
			(reflow "This expression produces a value, but it's not being used."))
		(document
			(source-region (file "record_different_fields_error.md") (start 4 5) (end 4 15) (annotation error) (line-text "    PascalCase: \"pascal\","))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[PascalCase, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Since this expression is used as a statement, it must evaluate to")
			(reflow " ")
			(annotated code "{}")
			(reflow ".")
			(line-break)
			(reflow "If you don't need the value, you can ignore it with")
			(reflow " ")
			(annotated code "_ =")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 4 17) (end 4 25))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "record_different_fields_error.md") (start 4 17) (end 4 25) (annotation error) (line-text "    PascalCase: \"pascal\","))
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
		(region (start 5 17) (end 5 24))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "record_different_fields_error.md") (start 5 17) (end 5 24) (annotation error) (line-text "    kebab-case: \"kebab\","))
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
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 6 5) (end 6 21))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "record_different_fields_error.md") (start 6 5) (end 6 21) (annotation error) (line-text "    field$special: \"dollar\","))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 7 19) (end 7 30))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "record_different_fields_error.md") (start 7 19) (end 7 30) (annotation error) (line-text "    field@symbol: \"at symbol\","))
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
NamedUnderscore,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
UpperIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpUnaryMinus,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpaqueName,OpColon,StringStart,StringPart,StringEnd,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-type-anno (name "_privateField")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(s-type-anno (name "field_")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-tag (raw "PascalCase"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-string
			(e-string-part (raw "pascal")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-ident (raw "kebab"))
		(unary "-"
			(e-ident (raw "case")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-string
			(e-string-part (raw "kebab")))
		(e-malformed (reason "expr_unexpected_token"))
		(s-type-anno (name "field$special")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-ident (raw "field"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-string
			(e-string-part (raw "at symbol")))
		(e-malformed (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
{
	_privateField :
			
	field_ :
			
	PascalCase
		"pascal"
	
	kebab
	-case
		"kebab"
	
	field$special :
			
	field
			"at symbol"
	
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "_privateField"))
		(e-anno-only))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "field_"))
		(e-anno-only))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-tag (name "PascalCase")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-expr
		(e-unary-minus
			(e-runtime-error (tag "ident_not_in_scope"))))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "field$special"))
		(e-anno-only))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "erroneous_value_expr")))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
