# META
~~~ini
description=Unicode single quotes
type=snippet
~~~
# SOURCE
~~~roc
x = (
    'a',
    'é',
    '🚀',
    '\u',
    '\u)',
    '\u(',
    '\u()',
    '\u(1F680)',
    '\u(EDA0B5)'
    '\u(K)',
    '\\',
    '\'',
    '',
    'long',
    '\',
)

y = 'u

# Test backslash before EOF
'\
~~~
# EXPECTED
INVALID UNICODE ESCAPE SEQUENCE - unicode_single_quotes.md:5:6:5:8
INVALID UNICODE ESCAPE SEQUENCE - unicode_single_quotes.md:6:6:6:8
INVALID UNICODE ESCAPE SEQUENCE - unicode_single_quotes.md:7:6:7:9
INVALID UNICODE ESCAPE SEQUENCE - unicode_single_quotes.md:8:6:8:10
INVALID UNICODE ESCAPE SEQUENCE - unicode_single_quotes.md:10:6:10:16
INVALID UNICODE ESCAPE SEQUENCE - unicode_single_quotes.md:11:6:11:11
SINGLE QUOTE EMPTY - unicode_single_quotes.md:14:5:14:7
SINGLE QUOTE TOO LONG - unicode_single_quotes.md:15:5:15:11
UNCLOSED SINGLE QUOTE - unicode_single_quotes.md:16:5:16:9
UNCLOSED SINGLE QUOTE - unicode_single_quotes.md:19:5:19:7
INVALID ESCAPE SEQUENCE - unicode_single_quotes.md:22:2:23:1
UNCLOSED SINGLE QUOTE - unicode_single_quotes.md:22:1:22:3
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:5:5:5:9
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:6:5:6:10
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:7:5:7:10
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:8:5:8:11
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:10:5:10:17
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:11:5:11:12
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:14:5:14:7
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:15:5:15:11
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:16:5:16:9
UNEXPECTED EXPRESSION SYNTAX - unicode_single_quotes.md:19:5:19:7
UNEXPECTED STATEMENT - unicode_single_quotes.md:22:1:22:3
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
INVALID TUPLE ELEMENT - :0:0:0:0
UNRECOGNIZED SYNTAX - unicode_single_quotes.md:19:5:19:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Unicode Escape Sequence")
		(region (start 5 6) (end 5 8))
		(headline
			(reflow "This Unicode escape sequence is not valid."))
		(document
			(source-region (file "unicode_single_quotes.md") (start 5 6) (end 5 8) (annotation error) (line-text "    '\\u',"))))
	(report
		(severity runtime_error)
		(title "Invalid Unicode Escape Sequence")
		(region (start 6 6) (end 6 8))
		(headline
			(reflow "This Unicode escape sequence is not valid."))
		(document
			(source-region (file "unicode_single_quotes.md") (start 6 6) (end 6 8) (annotation error) (line-text "    '\\u)',"))))
	(report
		(severity runtime_error)
		(title "Invalid Unicode Escape Sequence")
		(region (start 7 6) (end 7 9))
		(headline
			(reflow "This Unicode escape sequence is not valid."))
		(document
			(source-region (file "unicode_single_quotes.md") (start 7 6) (end 7 9) (annotation error) (line-text "    '\\u(',"))))
	(report
		(severity runtime_error)
		(title "Invalid Unicode Escape Sequence")
		(region (start 8 6) (end 8 10))
		(headline
			(reflow "This Unicode escape sequence is not valid."))
		(document
			(source-region (file "unicode_single_quotes.md") (start 8 6) (end 8 10) (annotation error) (line-text "    '\\u()',"))))
	(report
		(severity runtime_error)
		(title "Invalid Unicode Escape Sequence")
		(region (start 10 6) (end 10 16))
		(headline
			(reflow "This Unicode escape sequence is not valid."))
		(document
			(source-region (file "unicode_single_quotes.md") (start 10 6) (end 10 16) (annotation error) (line-text "    '\\u(EDA0B5)'"))))
	(report
		(severity runtime_error)
		(title "Invalid Unicode Escape Sequence")
		(region (start 11 6) (end 11 11))
		(headline
			(reflow "This Unicode escape sequence is not valid."))
		(document
			(source-region (file "unicode_single_quotes.md") (start 11 6) (end 11 11) (annotation error) (line-text "    '\\u(K)',"))))
	(report
		(severity runtime_error)
		(title "Single Quote Empty")
		(region (start 14 5) (end 14 7))
		(headline
			(reflow "Single-quoted literals must contain exactly one valid UTF-8 codepoint."))
		(document
			(source-region (file "unicode_single_quotes.md") (start 14 5) (end 14 7) (annotation error) (line-text "    '',"))))
	(report
		(severity runtime_error)
		(title "Single Quote Too Long")
		(region (start 15 5) (end 15 11))
		(headline
			(reflow "Single-quoted literals must contain exactly one valid UTF-8 codepoint."))
		(document
			(source-region (file "unicode_single_quotes.md") (start 15 5) (end 15 11) (annotation error) (line-text "    'long',"))))
	(report
		(severity runtime_error)
		(title "Unclosed Single Quote")
		(region (start 16 5) (end 16 9))
		(headline
			(reflow "This single-quoted literal is missing a closing quote."))
		(document
			(source-region (file "unicode_single_quotes.md") (start 16 5) (end 16 9) (annotation error) (line-text "    '\\',"))))
	(report
		(severity runtime_error)
		(title "Unclosed Single Quote")
		(region (start 19 5) (end 19 7))
		(headline
			(reflow "This single-quoted literal is missing a closing quote."))
		(document
			(source-region (file "unicode_single_quotes.md") (start 19 5) (end 19 7) (annotation error) (line-text "y = 'u"))))
	(report
		(severity runtime_error)
		(title "Invalid Escape Sequence")
		(region (start 22 2) (end 23 1))
		(headline
			(reflow "This escape sequence is not recognized."))
		(document
			(source-region (file "unicode_single_quotes.md") (start 22 2) (end 23 1) (annotation error) (line-text "'\\\n"))))
	(report
		(severity runtime_error)
		(title "Unclosed Single Quote")
		(region (start 22 1) (end 22 3))
		(headline
			(reflow "This single-quoted literal is missing a closing quote."))
		(document
			(source-region (file "unicode_single_quotes.md") (start 22 1) (end 22 3) (annotation error) (line-text "'\\"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 5 5) (end 5 9))
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
			(annotated code "'\\u'")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "unicode_single_quotes.md") (start 5 5) (end 5 9) (annotation error) (line-text "    '\\u',"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 6 5) (end 6 10))
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
			(annotated code "'\\u)'")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "unicode_single_quotes.md") (start 6 5) (end 6 10) (annotation error) (line-text "    '\\u)',"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 7 5) (end 7 10))
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
			(annotated code "'\\u('")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "unicode_single_quotes.md") (start 7 5) (end 7 10) (annotation error) (line-text "    '\\u(',"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 8 5) (end 8 11))
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
			(annotated code "'\\u()'")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "unicode_single_quotes.md") (start 8 5) (end 8 11) (annotation error) (line-text "    '\\u()',"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 10 5) (end 10 17))
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
			(annotated code "'\\u(EDA0B5)'")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "unicode_single_quotes.md") (start 10 5) (end 10 17) (annotation error) (line-text "    '\\u(EDA0B5)'"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 11 5) (end 11 12))
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
			(annotated code "'\\u(K)'")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "unicode_single_quotes.md") (start 11 5) (end 11 12) (annotation error) (line-text "    '\\u(K)',"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 14 5) (end 14 7))
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
			(annotated code "''")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "unicode_single_quotes.md") (start 14 5) (end 14 7) (annotation error) (line-text "    '',"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 15 5) (end 15 11))
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
			(annotated code "'long'")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "unicode_single_quotes.md") (start 15 5) (end 15 11) (annotation error) (line-text "    'long',"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 16 5) (end 16 9))
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
			(annotated code "'\\',")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "unicode_single_quotes.md") (start 16 5) (end 16 9) (annotation error) (line-text "    '\\',"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 19 5) (end 19 7))
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
			(annotated code "'u")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "unicode_single_quotes.md") (start 19 5) (end 19 7) (annotation error) (line-text "y = 'u"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 22 1) (end 22 3))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "'\\")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "unicode_single_quotes.md") (start 22 1) (end 22 3) (annotation error) (line-text "'\\"))))
	(report
		(severity runtime_error)
		(title "Invalid Tuple Element")
		(headline
			(reflow "This tuple element is malformed or contains invalid syntax."))
		(document))
	(report
		(severity runtime_error)
		(title "Invalid Tuple Element")
		(headline
			(reflow "This tuple element is malformed or contains invalid syntax."))
		(document))
	(report
		(severity runtime_error)
		(title "Invalid Tuple Element")
		(headline
			(reflow "This tuple element is malformed or contains invalid syntax."))
		(document))
	(report
		(severity runtime_error)
		(title "Invalid Tuple Element")
		(headline
			(reflow "This tuple element is malformed or contains invalid syntax."))
		(document))
	(report
		(severity runtime_error)
		(title "Invalid Tuple Element")
		(headline
			(reflow "This tuple element is malformed or contains invalid syntax."))
		(document))
	(report
		(severity runtime_error)
		(title "Invalid Tuple Element")
		(headline
			(reflow "This tuple element is malformed or contains invalid syntax."))
		(document))
	(report
		(severity runtime_error)
		(title "Invalid Tuple Element")
		(headline
			(reflow "This tuple element is malformed or contains invalid syntax."))
		(document))
	(report
		(severity runtime_error)
		(title "Invalid Tuple Element")
		(headline
			(reflow "This tuple element is malformed or contains invalid syntax."))
		(document))
	(report
		(severity runtime_error)
		(title "Invalid Tuple Element")
		(headline
			(reflow "This tuple element is malformed or contains invalid syntax."))
		(document))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 19 5) (end 19 7))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "unicode_single_quotes.md") (start 19 5) (end 19 7) (annotation error) (line-text "y = 'u"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenRound,
SingleQuote,Comma,
SingleQuote,Comma,
SingleQuote,Comma,
MalformedSingleQuote,Comma,
MalformedSingleQuote,Comma,
MalformedSingleQuote,Comma,
MalformedSingleQuote,Comma,
SingleQuote,Comma,
MalformedSingleQuote,
MalformedSingleQuote,Comma,
SingleQuote,Comma,
SingleQuote,Comma,
MalformedSingleQuote,Comma,
MalformedSingleQuote,Comma,
MalformedSingleQuote,
CloseRound,
LowerIdent,OpAssign,MalformedSingleQuote,
MalformedSingleQuote,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-tuple
				(e-single-quote (raw "'a'"))
				(e-single-quote (raw "'é'"))
				(e-single-quote (raw "'🚀'"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-single-quote (raw "'\u(1F680)'"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-single-quote (raw "'\\'"))
				(e-single-quote (raw "'\''"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-malformed (reason "expr_unexpected_token"))))
		(s-decl
			(p-ident (raw "y"))
			(e-malformed (reason "expr_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
x = ('a', 'é', '🚀', , , , , '\u(1F680)', , , '\\', '\'', , , )

y =

# Test backslash before EOF
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "y"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Dec, Dec, Dec, Error, Error, Error, Error, Dec, Error, Error, Dec, Dec, Error, Error, Error)"))
		(patt (type "Error")))
	(expressions
		(expr (type "(Dec, Dec, Dec, Error, Error, Error, Error, Dec, Error, Error, Dec, Dec, Error, Error, Error)"))
		(expr (type "Error"))))
~~~
