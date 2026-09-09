# META
~~~ini
description=An else branch of `{ x }` stays a block evaluating to x, not a record (issue #9723)
type=expr
~~~
# SOURCE
~~~roc
{
	x = 5
	if True {
		x
	} else {
		x
	}
}
~~~
# EXPECTED
UNCONDITIONAL CONDITION - single_field_record_else_single_brace.md:3:5:3:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unconditional Condition")
		(region (start 3 5) (end 3 9))
		(headline
			(reflow "This")
			(reflow " ")
			(reflow "if condition")
			(reflow " ")
			(reflow "is known at compile time, so")
			(reflow " ")
			(reflow "this conditional will always make the same choice."))
		(document
			(source-region (file "single_field_record_else_single_brace.md") (start 3 5) (end 3 9) (annotation warning) (line-text "\tif True {")))))
~~~
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Int,
KwIf,UpperIdent,OpenCurly,
LowerIdent,
CloseCurly,KwElse,OpenCurly,
LowerIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))
		(e-if-then-else
			(e-tag (raw "True"))
			(e-block
				(statements
					(e-ident (raw "x"))))
			(e-block
				(statements
					(e-ident (raw "x")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(e-if
		(if-branches
			(if-branch
				(e-tag (name "True"))
				(e-block
					(e-lookup-local
						(p-assign (ident "x"))))))
		(if-else
			(e-block
				(e-lookup-local
					(p-assign (ident "x")))))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
