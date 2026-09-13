# META
~~~ini
description=Unknown operator, should produce an error
type=expr
~~~
# SOURCE
~~~roc
1 ++ 2
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - unknown_operator.md:1:4:1:5
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 1 4) (end 1 5))
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
			(annotated code "+")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "unknown_operator.md") (start 1 4) (end 1 5) (annotation error) (line-text "1 ++ 2")))))
~~~
# TOKENS
~~~zig
Int,OpPlus,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "+")
	(e-int (raw "1"))
	(e-malformed (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
1 +
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
