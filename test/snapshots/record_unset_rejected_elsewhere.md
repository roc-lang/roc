# META
~~~ini
description=Bare `_` stays rejected in expression position outside a record field value
type=snippet
~~~
# SOURCE
~~~roc
x = _

y = { a: _ + 1 }
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - record_unset_rejected_elsewhere.md:1:5:1:6
UNEXPECTED EXPRESSION SYNTAX - record_unset_rejected_elsewhere.md:3:10:3:11
UNRECOGNIZED SYNTAX - record_unset_rejected_elsewhere.md:1:5:1:6
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 1 5) (end 1 6))
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
			(annotated code "_")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_unset_rejected_elsewhere.md") (start 1 5) (end 1 6) (annotation error) (line-text "x = _"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 3 10) (end 3 11))
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
			(annotated code "_")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "record_unset_rejected_elsewhere.md") (start 3 10) (end 3 11) (annotation error) (line-text "y = { a: _ + 1 }"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 1 5) (end 1 6))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "record_unset_rejected_elsewhere.md") (start 1 5) (end 1 6) (annotation error) (line-text "x = _"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,Underscore,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Underscore,OpPlus,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-malformed (reason "expr_unexpected_token")))
		(s-decl
			(p-ident (raw "y"))
			(e-record
				(field (field "a")
					(e-binop (op "+")
						(e-malformed (reason "expr_unexpected_token"))
						(e-int (raw "1"))))))))
~~~
# FORMATTED
~~~roc
x =

y = { a:  + 1 }
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign (ident "y"))
		(e-record
			(fields))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "{}")))
	(expressions
		(expr (type "Error"))
		(expr (type "{}"))))
~~~
