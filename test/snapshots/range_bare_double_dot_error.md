# META
~~~ini
description=Bare .. suggests ..< or ..=
type=snippet
~~~
# SOURCE
~~~roc
r = 1..5
~~~
# EXPECTED
NOT A RANGE OPERATOR - range_bare_double_dot_error.md:1:8:1:9
UNRECOGNIZED SYNTAX - range_bare_double_dot_error.md:1:8:1:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Not A Range Operator")
		(region (start 1 8) (end 1 9))
		(headline
			(reflow "I was parsing an expression, and `..` is not a range operator."))
		(document
			(reflow "Use ")
			(annotated code "..<")
			(reflow " for an exclusive range or ")
			(annotated code "..=")
			(reflow " for an inclusive range.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "1..<10")
			(line-break)
			(indent 1)
			(text "1..=10")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "range_bare_double_dot_error.md") (start 1 8) (end 1 9) (annotation error) (line-text "r = 1..5"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 1 8) (end 1 9))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "range_bare_double_dot_error.md") (start 1 8) (end 1 9) (annotation error) (line-text "r = 1..5"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,DoubleDot,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "r"))
			(e-malformed (reason "expr_double_dot_is_not_range")))))
~~~
# FORMATTED
~~~roc
r =
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
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
