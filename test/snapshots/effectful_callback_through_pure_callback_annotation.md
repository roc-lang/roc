# META
~~~ini
description=A pure callback annotation makes the functions an effect-polymorphic lambda calls pure
type=snippet
~~~
# SOURCE
~~~roc
go! : Str => Str
go! = |s| s

call_with : (Str -> Str) -> Str
call_with = |k| k("z")

apply = |x, f| call_with(|_| f(x))

expect apply("a", go!) == "a"
~~~
# EXPECTED
TYPE MISMATCH - effectful_callback_through_pure_callback_annotation.md:9:8:9:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 9 8) (end 9 23))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "argument being passed to this function has the wrong type."))
		(document
			(source-underlines
				(display (file "effectful_callback_through_pure_callback_annotation.md") (start 9 8) (end 9 23) (annotation dim) (line-text "expect apply(\"a\", go!) == \"a\""))
				(underline (start 9 19) (end 9 22) (annotation error)))
			(line-break)
			(reflow "This argument has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str => Str")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But")
			(reflow " ")
			(annotated code "apply")
			(reflow " ")
			(reflow "needs the")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "argument to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a -> Str where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This function is effectful, but a pure function is expected."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpColon,OpenRound,UpperIdent,OpArrow,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,OpBar,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
KwExpect,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,LowerIdent,CloseRound,OpEquals,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "go!")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "go!"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-ident (raw "s"))))
		(s-type-anno (name "call_with")
			(ty-fn
				(ty-fn
					(ty (name "Str"))
					(ty (name "Str")))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "call_with"))
			(e-lambda
				(args
					(p-ident (raw "k")))
				(e-apply
					(e-ident (raw "k"))
					(e-string
						(e-string-part (raw "z"))))))
		(s-decl
			(p-ident (raw "apply"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "f")))
				(e-apply
					(e-ident (raw "call_with"))
					(e-lambda
						(args
							(p-underscore))
						(e-apply
							(e-ident (raw "f"))
							(e-ident (raw "x")))))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "apply"))
					(e-string
						(e-string-part (raw "a")))
					(e-ident (raw "go!")))
				(e-string
					(e-string-part (raw "a")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "go!"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-lookup-local
				(p-assign (ident "s"))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "call_with"))
		(e-lambda
			(args
				(p-assign (ident "k")))
			(e-call (constraint-fn-var 285)
				(e-lookup-local
					(p-assign (ident "k")))
				(e-string
					(e-literal (string "z")))))
		(annotation
			(ty-fn (effectful false)
				(ty-parens
					(ty-fn (effectful false)
						(ty-lookup (name "Str") (builtin))
						(ty-lookup (name "Str") (builtin))))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "apply"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "f")))
			(e-call (constraint-fn-var 296)
				(e-lookup-local
					(p-assign (ident "call_with")))
				(e-closure
					(captures
						(capture (ident "f"))
						(capture (ident "x")))
					(e-lambda
						(args
							(p-underscore))
						(e-call (constraint-fn-var 295)
							(e-lookup-local
								(p-assign (ident "f")))
							(e-lookup-local
								(p-assign (ident "x")))))))))
	(s-expect
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => Str"))
		(patt (type "(Str -> Str) -> Str"))
		(patt (type "a, (a -> Str) -> Str")))
	(expressions
		(expr (type "Str => Str"))
		(expr (type "(Str -> Str) -> Str"))
		(expr (type "a, (a -> Str) -> Str"))))
~~~
