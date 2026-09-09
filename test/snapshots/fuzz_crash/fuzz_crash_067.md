# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
f = || {
    crash 1
}
~~~
# EXPECTED
TYPE MISMATCH - fuzz_crash_067.md:2:11:2:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 11) (end 2 12))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "fuzz_crash_067.md") (start 2 11) (end 2 12) (annotation error) (line-text "    crash 1"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
KwCrash,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-crash
							(e-int (raw "1")))))))))
~~~
# FORMATTED
~~~roc
f = || {
	crash 1
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args)
			(e-block
				(e-run-low-level (op "crash")
					(args
						(e-runtime-error (tag "erroneous_value_expr"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> _ret")))
	(expressions
		(expr (type "({}) -> _ret"))))
~~~
