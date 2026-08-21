# META
~~~ini
description=two strings
type=snippet
~~~
# SOURCE
~~~roc
x = (
	"one",
	"two",
	"\u",
	"\u)",
	"\u(",
	"\u()",
	"\u(K)",
	"\u(1F680)",
)

# Test backslash before EOF
"\
~~~
# EXPECTED
INVALID UNICODE ESCAPE SEQUENCE - string.md:4:3:4:5
INVALID UNICODE ESCAPE SEQUENCE - string.md:5:3:5:5
INVALID UNICODE ESCAPE SEQUENCE - string.md:6:3:6:6
INVALID UNICODE ESCAPE SEQUENCE - string.md:7:3:7:7
INVALID UNICODE ESCAPE SEQUENCE - string.md:8:3:8:8
INVALID ESCAPE SEQUENCE - string.md:13:2:14:1
UNCLOSED STRING - string.md:13:1:13:3
UNEXPECTED STATEMENT - string.md:13:1:13:2
UNEXPECTED STATEMENT - string.md:13:2:13:3
UNEXPECTED STATEMENT - string.md:13:3:13:3
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Unicode Escape Sequence")
		(region (start 4 3) (end 4 5))
		(headline
			(reflow "This Unicode escape sequence is not valid."))
		(document
			(source-region (file "string.md") (start 4 3) (end 4 5) (annotation error) (line-text "\t\"\\u\","))))
	(report
		(severity runtime_error)
		(title "Invalid Unicode Escape Sequence")
		(region (start 5 3) (end 5 5))
		(headline
			(reflow "This Unicode escape sequence is not valid."))
		(document
			(source-region (file "string.md") (start 5 3) (end 5 5) (annotation error) (line-text "\t\"\\u)\","))))
	(report
		(severity runtime_error)
		(title "Invalid Unicode Escape Sequence")
		(region (start 6 3) (end 6 6))
		(headline
			(reflow "This Unicode escape sequence is not valid."))
		(document
			(source-region (file "string.md") (start 6 3) (end 6 6) (annotation error) (line-text "\t\"\\u(\","))))
	(report
		(severity runtime_error)
		(title "Invalid Unicode Escape Sequence")
		(region (start 7 3) (end 7 7))
		(headline
			(reflow "This Unicode escape sequence is not valid."))
		(document
			(source-region (file "string.md") (start 7 3) (end 7 7) (annotation error) (line-text "\t\"\\u()\","))))
	(report
		(severity runtime_error)
		(title "Invalid Unicode Escape Sequence")
		(region (start 8 3) (end 8 8))
		(headline
			(reflow "This Unicode escape sequence is not valid."))
		(document
			(source-region (file "string.md") (start 8 3) (end 8 8) (annotation error) (line-text "\t\"\\u(K)\","))))
	(report
		(severity runtime_error)
		(title "Invalid Escape Sequence")
		(region (start 13 2) (end 14 1))
		(headline
			(reflow "This escape sequence is not recognized."))
		(document
			(source-region (file "string.md") (start 13 2) (end 14 1) (annotation error) (line-text "\"\\\n"))))
	(report
		(severity runtime_error)
		(title "Unclosed String")
		(region (start 13 1) (end 13 3))
		(headline
			(reflow "This string is missing a closing quote."))
		(document
			(source-region (file "string.md") (start 13 1) (end 13 3) (annotation error) (line-text "\"\\"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 1) (end 13 2))
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
			(annotated code "\"")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "string.md") (start 13 1) (end 13 2) (annotation error) (line-text "\"\\"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 2) (end 13 3))
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
			(annotated code "\\")
			(text " here.")
			(line-break)
			(line-break)
			(text "Tip: ")
			(reflow "Roc syntax does not use single backslashes. Roc lambda syntax is ")
			(annotated code "|arg1, arg2| body")
			(reflow ", and double backslash (")
			(annotated code "\\\\")
			(reflow ") begins a line in a multiline string.")
			(line-break)
			(line-break)
			(source-region (file "string.md") (start 13 2) (end 13 3) (annotation error) (line-text "\"\\"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 3) (end 13 3))
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
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "string.md") (start 13 3) (end 13 3) (annotation error) (line-text "\"\\")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenRound,
StringStart,StringPart,StringEnd,Comma,
StringStart,StringPart,StringEnd,Comma,
StringStart,MalformedStringPart,StringEnd,Comma,
StringStart,MalformedStringPart,StringEnd,Comma,
StringStart,MalformedStringPart,StringEnd,Comma,
StringStart,MalformedStringPart,StringEnd,Comma,
StringStart,MalformedStringPart,StringEnd,Comma,
StringStart,StringPart,StringEnd,Comma,
CloseRound,
StringStart,MalformedStringPart,StringEnd,
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
				(e-string
					(e-string-part (raw "one")))
				(e-string
					(e-string-part (raw "two")))
				(e-string)
				(e-string)
				(e-string)
				(e-string)
				(e-string)
				(e-string
					(e-string-part (raw "\u(1F680)")))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
x = (
	"one",
	"two",
	"",
	"",
	"",
	"",
	"",
	"\u(1F680)",
)

# Test backslash before EOF
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-tuple
			(elems
				(e-string
					(e-literal (string "one")))
				(e-string
					(e-literal (string "two")))
				(e-string)
				(e-string)
				(e-string)
				(e-string)
				(e-string)
				(e-string
					(e-literal (string "🚀")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Str, Str, Str, Str, Str, Str, Str, Str)")))
	(expressions
		(expr (type "(Str, Str, Str, Str, Str, Str, Str, Str)"))))
~~~
