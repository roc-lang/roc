# META
~~~ini
description=An unused statement value does not poison the callee's return type, so later uses of the callee still report their own errors
type=snippet
~~~
# SOURCE
~~~roc
run : (Str -> Str) -> Str
run = |f| {
    f("hello")
    y = f("world")
    y.concat(1)
}
~~~
# EXPECTED
TYPE MISMATCH - unused_statement_value_keeps_callee_type.md:3:5:3:15
TYPE MISMATCH - unused_statement_value_keeps_callee_type.md:5:14:5:15
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 3 5) (end 3 15))
		(headline
			(reflow "This expression produces a value, but it's not being used."))
		(document
			(source-region (file "unused_statement_value_keeps_callee_type.md") (start 3 5) (end 3 15) (annotation error) (line-text "    f(\"hello\")"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Since this expression is used as a statement, it must evaluate to")
			(reflow " ")
			(annotated code "{}")
			(reflow ".")
			(line-break)
			(reflow "If you don't need the value, you can ignore it with")
			(reflow " ")
			(annotated code "_ =")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 5 14) (end 5 15))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "unused_statement_value_keeps_callee_type.md") (start 5 14) (end 5 15) (annotation error) (line-text "    y.concat(1)"))
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
LowerIdent,OpColon,OpenRound,UpperIdent,OpArrow,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "run")
			(ty-fn
				(ty-fn
					(ty (name "Str"))
					(ty (name "Str")))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "run"))
			(e-lambda
				(args
					(p-ident (raw "f")))
				(e-block
					(statements
						(e-apply
							(e-ident (raw "f"))
							(e-string
								(e-string-part (raw "hello"))))
						(s-decl
							(p-ident (raw "y"))
							(e-apply
								(e-ident (raw "f"))
								(e-string
									(e-string-part (raw "world")))))
						(e-method-call (method ".concat")
							(receiver
								(e-ident (raw "y")))
							(args
								(e-int (raw "1"))))))))))
~~~
# FORMATTED
~~~roc
run : (Str -> Str) -> Str
run = |f| {
	f("hello")
	y = f("world")
	y.concat(1)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "run"))
		(e-lambda
			(args
				(p-assign (ident "f")))
			(e-block
				(s-expr
					(e-runtime-error (tag "erroneous_value_expr")))
				(s-let
					(p-assign (ident "y"))
					(e-call (constraint-fn-var 262)
						(e-lookup-local
							(p-assign (ident "f")))
						(e-string
							(e-literal (string "world")))))
				(e-dispatch-call (method "concat") (constraint-fn-var 264)
					(receiver
						(e-lookup-local
							(p-assign (ident "y"))))
					(args
						(e-runtime-error (tag "erroneous_value_expr"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-parens
					(ty-fn (effectful false)
						(ty-lookup (name "Str") (builtin))
						(ty-lookup (name "Str") (builtin))))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Str -> Str) -> Str")))
	(expressions
		(expr (type "(Str -> Str) -> Str"))))
~~~
