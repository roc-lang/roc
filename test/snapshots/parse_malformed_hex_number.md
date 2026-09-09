# META
~~~ini
description=Malformed hex number (0x without digits)
type=expr
~~~
# SOURCE
~~~roc
0x
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - parse_malformed_hex_number.md:1:1:1:3
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 1 1) (end 1 3))
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
			(annotated code "0x")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "parse_malformed_hex_number.md") (start 1 1) (end 1 3) (annotation error) (line-text "0x")))))
~~~
# TOKENS
~~~zig
MalformedNumberNoDigits,
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
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
