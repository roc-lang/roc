# META
~~~ini
description=parser formatter output instability (stable idempotence)
type=file
~~~
# SOURCE
~~~roc
a=(0->b .c())
~~~
# EXPECTED
NAME NOT IN SCOPE - fuzz_crash_087.md:1:7:1:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 1 7) (end 1 8))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "b")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_087.md") (start 1 7) (end 1 8) (annotation error) (line-text "a=(0->b .c())")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,NoSpaceOpenRound,Int,OpArrow,LowerIdent,DotLowerIdent,NoSpaceOpenRound,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-tuple
				(e-method-call (method ".c")
					(receiver
						(e-arrow-call
							(e-int (raw "0"))
							(e-ident (raw "b"))))
					(args))))))
~~~
# FORMATTED
~~~roc
a = ((0 |> b).c())
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
