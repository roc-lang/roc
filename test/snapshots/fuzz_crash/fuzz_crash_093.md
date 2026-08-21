# META
~~~ini
description=Issue #10047: Unstable formatting for parser tuple vs lambda grouping
type=file
~~~
# SOURCE
~~~roc
d=(0||())
~~~
# EXPECTED
EMPTY TUPLE NOT ALLOWED - fuzz_crash_093.md:1:7:1:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Empty Tuple Not Allowed")
		(region (start 1 7) (end 1 9))
		(headline
			(reflow "I am part way through parsing this tuple, but it is empty."))
		(document
			(source-region (file "fuzz_crash_093.md") (start 1 7) (end 1 9) (annotation error) (line-text "d=(0||())"))
			(line-break)
			(reflow "If you want to represent nothing, try using an empty record: ")
			(annotated code "{}")
			(reflow "."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,NoSpaceOpenRound,Int,OpBar,OpBar,NoSpaceOpenRound,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "d"))
			(e-tuple
				(e-int (raw "0"))
				(e-lambda
					(args)
					(e-tuple))))))
~~~
# FORMATTED
~~~roc
d = (0, || ())
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "d"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Dec, ({}) -> Error)")))
	(expressions
		(expr (type "(Dec, ({}) -> Error)"))))
~~~
