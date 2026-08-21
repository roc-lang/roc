# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
#el
var t= ]

#el
var t= 0
~~~
# EXPECTED
VAR OUTSIDE BODY - fuzz_crash_024.md:2:1:2:4
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_024.md:2:8:2:9
VAR OUTSIDE BODY - fuzz_crash_024.md:5:1:5:4
UNRECOGNIZED SYNTAX - fuzz_crash_024.md:2:8:2:9
DUPLICATE DEFINITION - fuzz_crash_024.md:5:5:5:6
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Var Outside Body")
		(region (start 2 1) (end 2 4))
		(headline
			(reflow "I was parsing a statement, and `var` appeared outside a function or block body."))
		(document
			(reflow "Mutable variables are local body statements. Move this ")
			(annotated code "var")
			(reflow " into a body, or use an ordinary top-level declaration.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "main = {")
			(line-break)
			(indent 1)
			(text "    var count = 0")
			(line-break)
			(indent 1)
			(text "    count")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "var")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_024.md") (start 2 1) (end 2 4) (annotation error) (line-text "var t= ]"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 2 8) (end 2 9))
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
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_024.md") (start 2 8) (end 2 9) (annotation error) (line-text "var t= ]"))))
	(report
		(severity runtime_error)
		(title "Var Outside Body")
		(region (start 5 1) (end 5 4))
		(headline
			(reflow "I was parsing a statement, and `var` appeared outside a function or block body."))
		(document
			(reflow "Mutable variables are local body statements. Move this ")
			(annotated code "var")
			(reflow " into a body, or use an ordinary top-level declaration.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "main = {")
			(line-break)
			(indent 1)
			(text "    var count = 0")
			(line-break)
			(indent 1)
			(text "    count")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "var")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_024.md") (start 5 1) (end 5 4) (annotation error) (line-text "var t= 0"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 2 8) (end 2 9))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "fuzz_crash_024.md") (start 2 8) (end 2 9) (annotation error) (line-text "var t= ]"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 5 5) (end 5 6))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "t")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "fuzz_crash_024.md") (start 5 5) (end 5 6) (annotation error) (line-text "var t= 0"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "t")
			(reflow " was already defined in ")
			(source-location
				(file "fuzz_crash_024.md")
				(line 2)
				(column 5))
			(reflow ":")
			(line-break)
			(source-region (file "fuzz_crash_024.md") (start 2 5) (end 2 6) (annotation dim) (line-text "var t= ]")))))
~~~
# TOKENS
~~~zig
KwVar,LowerIdent,OpAssign,CloseSquare,
KwVar,LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "var_only_allowed_in_a_body"))
		(s-decl
			(p-ident (raw "t"))
			(e-malformed (reason "expr_unexpected_token")))
		(s-malformed (tag "var_only_allowed_in_a_body"))
		(s-decl
			(p-ident (raw "t"))
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
# el
t =

# el
t = 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "t"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign (ident "t"))
		(e-num (value "0"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Dec")))
	(expressions
		(expr (type "Error"))
		(expr (type "Dec"))))
~~~
