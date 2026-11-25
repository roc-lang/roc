# META
~~~ini
description=A separator between the leading 0 and a base letter leaves no base prefix
type=expr
~~~
# SOURCE
~~~roc
0_x10
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - parse_number_separator_before_base_prefix.md:1:1:1:6
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 1 1) (end 1 6))
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
			(annotated code "0_x10")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "parse_number_separator_before_base_prefix.md") (start 1 1) (end 1 6) (annotation error) (line-text "0_x10")))))
~~~
# TOKENS
~~~zig
MalformedNumberBadSuffix,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expr_unexpected_token"))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "expr_syntax_error"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
