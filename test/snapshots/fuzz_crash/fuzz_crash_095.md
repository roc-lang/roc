# META
~~~ini
description=Issue #10095: Invalid formatting for arrow call on parenthesized expression
type=file
~~~
# SOURCE
~~~roc
t=0->(0)()
~~~
# EXPECTED
TYPE MISMATCH - fuzz_crash_095.md:1:7:1:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 7) (end 1 8))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "fuzz_crash_095.md") (start 1 7) (end 1 8) (annotation error) (line-text "t=0->(0)()"))
			(line-break)
			(reflow "The type was determined to be non-numeric here:")
			(line-break)
			(source-region (file "fuzz_crash_095.md") (start 1 3) (end 1 11) (annotation error) (line-text "t=0->(0)()"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "_arg -> _ret")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,OpArrow,NoSpaceOpenRound,Int,CloseRound,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "t"))
			(e-arrow-call
				(e-int (raw "0"))
				(e-apply
					(e-int (raw "0")))))))
~~~
# FORMATTED
~~~roc
t = 0 |> (0)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "t"))
		(e-call (constraint-fn-var 229)
			(e-runtime-error (tag "erroneous_value_expr"))
			(e-num (value "0")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a")))
	(expressions
		(expr (type "_a"))))
~~~
