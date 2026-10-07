# META
~~~ini
description=Equality (==) on unresolved type variables is rejected at check time without mentioning is_eq (issue 9485)
type=file
~~~
# SOURCE
~~~roc
poly = || { crash "x" }

result = poly() == poly()
~~~
# EXPECTED
TYPE NOT DETERMINED - static_dispatch_unresolved_equality.md:3:10:3:16
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Not Determined")
		(region (start 3 10) (end 3 16))
		(headline
			(reflow "Nothing in this program determines the type of the values this")
			(reflow " ")
			(annotated code "==")
			(reflow " ")
			(reflow "compares:"))
		(document
			(source-region (file "static_dispatch_unresolved_equality.md") (start 3 10) (end 3 16) (annotation error) (line-text "result = poly() == poly()"))
			(line-break)
			(reflow "Without knowing which type it is, there's no way to tell how to compare them.")
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Add a type annotation saying which type it should be."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,KwCrash,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,CloseRound,OpEquals,LowerIdent,NoSpaceOpenRound,CloseRound,
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
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "poly")))
				(e-apply
					(e-ident (raw "poly")))))))
~~~
# FORMATTED
~~~roc
poly = || {
	crash "x"
}

result = poly() == poly()
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
		(e-method-eq (negated "false")
			(lhs
				(e-runtime-error (tag "erroneous_value_expr")))
			(rhs
				(e-call (constraint-fn-var 233)
					(e-lookup-local
						(p-assign (ident "poly"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> _ret"))
		(patt (type "Bool")))
	(expressions
		(expr (type "({}) -> _ret"))
		(expr (type "Bool"))))
~~~
