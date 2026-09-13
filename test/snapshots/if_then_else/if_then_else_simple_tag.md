# META
~~~ini
description=Example if-then-else statement with a tag expression
type=expr
~~~
# SOURCE
~~~roc
if Bool.True Ok(0) else Err(1)
~~~
# EXPECTED
UNCONDITIONAL CONDITION - if_then_else_simple_tag.md:1:4:1:13
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
			(source-region (file "if_then_else_simple_tag.md") (start 1 4) (end 1 13) (annotation warning) (line-text "if Bool.True Ok(0) else Err(1)")))))
~~~
# TOKENS
~~~zig
KwIf,UpperIdent,NoSpaceDotUpperIdent,UpperIdent,NoSpaceOpenRound,Int,CloseRound,KwElse,UpperIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-tag (raw "Bool.True"))
	(e-apply
		(e-tag (raw "Ok"))
		(e-int (raw "0")))
	(e-apply
		(e-tag (raw "Err"))
		(e-int (raw "1"))))
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
			(e-tag (name "Ok")
				(args
					(e-num (value "0"))))))
	(if-else
		(e-tag (name "Err")
			(args
				(e-num (value "1"))))))
~~~
# TYPES
~~~clojure
(expr (type "[Err(Dec), Ok(Dec), ..]"))
~~~
