# META
~~~ini
description=fuzz crash, unterminated single quote
type=file
~~~
# SOURCE
~~~roc
mule []

#el
vavar t= '
~~~
# EXPECTED
UNCLOSED SINGLE QUOTE - fuzz_crash_031.md:4:10:4:11
UNEXPECTED STATEMENT - fuzz_crash_031.md:1:1:1:5
UNEXPECTED STATEMENT - fuzz_crash_031.md:1:6:1:7
UNEXPECTED STATEMENT - fuzz_crash_031.md:1:7:1:8
UNEXPECTED STATEMENT - fuzz_crash_031.md:4:1:4:6
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_031.md:4:10:4:11
UNRECOGNIZED SYNTAX - fuzz_crash_031.md:4:10:4:11
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unclosed Single Quote")
		(region (start 4 10) (end 4 11))
		(headline
			(reflow "This single-quoted literal is missing a closing quote."))
		(document
			(source-region (file "fuzz_crash_031.md") (start 4 10) (end 4 11) (annotation error) (line-text "vavar t= '"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 1) (end 1 5))
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
			(annotated code "mule")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_031.md") (start 1 1) (end 1 5) (annotation error) (line-text "mule []"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 6) (end 1 7))
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
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_031.md") (start 1 6) (end 1 7) (annotation error) (line-text "mule []"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 7) (end 1 8))
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
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_031.md") (start 1 7) (end 1 8) (annotation error) (line-text "mule []"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 4 1) (end 4 6))
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
			(annotated code "vavar")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_031.md") (start 4 1) (end 4 6) (annotation error) (line-text "vavar t= '"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 4 10) (end 4 11))
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
			(annotated code "'")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_031.md") (start 4 10) (end 4 11) (annotation error) (line-text "vavar t= '"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 4 10) (end 4 11))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "fuzz_crash_031.md") (start 4 10) (end 4 11) (annotation error) (line-text "vavar t= '"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpenSquare,CloseSquare,
LowerIdent,LowerIdent,OpAssign,MalformedSingleQuote,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "t"))
			(e-malformed (reason "expr_unexpected_token")))))
~~~
# FORMATTED
~~~roc


# el
t =
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "t"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
