# META
~~~ini
description=if else multiline without curly brackets
type=expr
~~~
# SOURCE
~~~roc
if Bool.True
	"true"
else
	"false"
~~~
# EXPECTED
UNCONDITIONAL CONDITION - if_then_else_multiline_no_curlies.md:1:4:1:13
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unconditional Condition")
		(region (start 1 4) (end 1 13))
		(headline
			(reflow "This")
			(reflow " ")
			(reflow "if condition")
			(reflow " ")
			(reflow "is known at compile time, so")
			(reflow " ")
			(reflow "this conditional will always make the same choice."))
		(document
			(source-region (file "if_then_else_multiline_no_curlies.md") (start 1 4) (end 1 13) (annotation warning) (line-text "if Bool.True")))))
~~~
# TOKENS
~~~zig
KwIf,UpperIdent,NoSpaceDotUpperIdent,
StringStart,StringPart,StringEnd,
KwElse,
StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-tag (raw "Bool.True"))
	(e-string
		(e-string-part (raw "true")))
	(e-string
		(e-string-part (raw "false"))))
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
			(e-nominal-external
				(builtin)
				(e-tag (name "True")))
			(e-string
				(e-literal (string "true")))))
	(if-else
		(e-string
			(e-literal (string "false")))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
