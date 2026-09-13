# META
~~~ini
description=if_then_else (9)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	1
} else if 10 { # Comment after else open
	A
} else { # Comment after else open
	3
}
~~~
# EXPECTED
UNCONDITIONAL CONDITION - if_then_else_9.md:3:11:3:13
TYPE MISMATCH - if_then_else_9.md:3:11:3:13
MISSING METHOD - if_then_else_9.md:2:2:2:3
MISSING METHOD - if_then_else_9.md:6:2:6:3
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unconditional Condition")
		(region (start 3 11) (end 3 13))
		(headline
			(reflow "This")
			(reflow " ")
			(reflow "if condition")
			(reflow " ")
			(reflow "is known at compile time, so")
			(reflow " ")
			(reflow "this conditional will always make the same choice."))
		(document
			(source-region (file "if_then_else_9.md") (start 3 11) (end 3 13) (annotation warning) (line-text "} else if 10 { # Comment after else open"))))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 3 11) (end 3 13))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "if_then_else_9.md") (start 3 11) (end 3 13) (annotation error) (line-text "} else if 10 { # Comment after else open"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Bool")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 2 2) (end 2 3))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_numeral")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "if_then_else_9.md") (start 2 2) (end 2 3) (annotation error) (line-text "\t1"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_numeral")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[A, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 6 2) (end 6 3))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_numeral")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "if_then_else_9.md") (start 6 2) (end 6 3) (annotation error) (line-text "\t3"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_numeral")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[A, ..]")
			(annotation-end))))
~~~
# TOKENS
~~~zig
KwIf,LowerIdent,OpenCurly,
Int,
CloseCurly,KwElse,KwIf,Int,OpenCurly,
UpperIdent,
CloseCurly,KwElse,OpenCurly,
Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-ident (raw "bool"))
	(e-block
		(statements
			(e-int (raw "1"))))
	(e-if-then-else
		(e-int (raw "10"))
		(e-block
			(statements
				(e-tag (raw "A"))))
		(e-block
			(statements
				(e-int (raw "3"))))))
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
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-block
				(e-runtime-error (tag "erroneous_value_expr"))))
		(if-branch
			(e-runtime-error (tag "erroneous_value_expr"))
			(e-block
				(e-tag (name "A")))))
	(if-else
		(e-block
			(e-runtime-error (tag "erroneous_value_expr")))))
~~~
# TYPES
~~~clojure
(expr (type "[A, ..]"))
~~~
