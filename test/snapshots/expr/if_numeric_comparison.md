# META
~~~ini
description=If expression with numeric comparison
type=expr
~~~
# SOURCE
~~~roc
if 5 > 3 1 else 2
~~~
# EXPECTED
UNCONDITIONAL CONDITION - if_numeric_comparison.md:1:4:1:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unconditional Condition")
		(region (start 1 4) (end 1 9))
		(headline
			(reflow "This")
			(reflow " ")
			(reflow "if condition")
			(reflow " ")
			(reflow "is known at compile time, so")
			(reflow " ")
			(reflow "this conditional will always make the same choice."))
		(document
			(source-region (file "if_numeric_comparison.md") (start 1 4) (end 1 9) (annotation warning) (line-text "if 5 > 3 1 else 2")))))
~~~
# TOKENS
~~~zig
KwIf,Int,OpGreaterThan,Int,Int,KwElse,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-binop (op ">")
		(e-int (raw "5"))
		(e-int (raw "3")))
	(e-int (raw "1"))
	(e-int (raw "2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if
	(if-branches
		(if-branch
			(e-dispatch-call (method "is_gt") (constraint-fn-var 222)
				(receiver
					(e-num (value "5")))
				(args
					(e-num (value "3"))))
			(e-num (value "1"))))
	(if-else
		(e-num (value "2"))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
