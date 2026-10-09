# META
~~~ini
description=Static dispatch of a method on an unresolved type variable is rejected at check time (issue 9485)
type=file
~~~
# SOURCE
~~~roc
poly = || { crash "x" }

result = poly().to_i128()
~~~
# EXPECTED
TYPE NOT DETERMINED - static_dispatch_unresolved_method.md:3:10:3:16
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Not Determined")
		(region (start 3 10) (end 3 16))
		(headline
			(reflow "Nothing in this program determines the type this")
			(reflow " ")
			(annotated code "to_i128")
			(reflow " ")
			(reflow "method is called on:"))
		(document
			(source-region (file "static_dispatch_unresolved_method.md") (start 3 10) (end 3 16) (annotation error) (line-text "result = poly().to_i128()"))
			(line-break)
			(reflow "Without knowing which type it is, there's no way to tell which")
			(reflow " ")
			(annotated code "to_i128")
			(reflow " ")
			(reflow "method to use.")
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Add a type annotation saying which type it should be."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,KwCrash,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "poly"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-crash
							(e-string
								(e-string-part (raw "x"))))))))
		(s-decl
			(p-ident (raw "result"))
			(e-method-call (method ".to_i128")
				(receiver
					(e-apply
						(e-ident (raw "poly"))))
				(args)))))
~~~
# FORMATTED
~~~roc
poly = || {
	crash "x"
}

result = poly().to_i128()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "poly"))
		(e-lambda
			(args)
			(e-block
				(e-run-low-level (op "crash")
					(args
						(e-string
							(e-literal (string "x"))))))))
	(d-let
		(p-assign (ident "result"))
		(e-dispatch-call (method "to_i128") (constraint-fn-var 229)
			(receiver
				(e-runtime-error (tag "erroneous_value_expr")))
			(args))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> _ret"))
		(patt (type "_a")))
	(expressions
		(expr (type "({}) -> _ret"))
		(expr (type "_a"))))
~~~
