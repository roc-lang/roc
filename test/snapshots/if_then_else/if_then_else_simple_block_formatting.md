# META
~~~ini
description=if_then_else (3)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	A
} else 2
~~~
# EXPECTED
TYPE MISMATCH - if_then_else_simple_block_formatting.md:3:8:3:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 3 8) (end 3 9))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "if_then_else_simple_block_formatting.md") (start 3 8) (end 3 9) (annotation error) (line-text "} else 2"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[A]")
			(annotation-end))))
~~~
# TOKENS
~~~zig
KwIf,LowerIdent,OpenCurly,
UpperIdent,
CloseCurly,KwElse,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-ident (raw "bool"))
	(e-block
		(statements
			(e-tag (raw "A"))))
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
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-block
				(e-tag (name "A")))))
	(if-else
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(expr (type "[A]"))
~~~
