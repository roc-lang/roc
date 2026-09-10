# META
~~~ini
description=An effectful function cannot flow into the pure callback of List.join_map through an effect-polymorphic helper
type=snippet
~~~
# SOURCE
~~~roc
apply = |lines, functions| lines.join_map(|line| functions.join_map(|f| f(line)))

pure_one = |line| [line]

shout! : Str => List(Str)
shout! = |line| [line]

expect apply(["a"], [pure_one, shout!]) |> List.is_eq(["a", "a"])
~~~
# EXPECTED
TYPE MISMATCH - effectful_callback_through_pure_join_map.md:8:21:8:39
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 8 21) (end 8 39))
		(headline
			(reflow "The")
			(reflow " ")
			(annotated code "join_map")
			(reflow " ")
			(reflow "method on")
			(reflow " ")
			(annotated code "List")
			(reflow " ")
			(reflow "has an incompatible type."))
		(document
			(source-region (file "effectful_callback_through_pure_join_map.md") (start 8 21) (end 8 39) (annotation error) (line-text "expect apply([\"a\"], [pure_one, shout!]) |> List.is_eq([\"a\", \"a\"])"))
			(line-break)
			(reflow "The method")
			(reflow " ")
			(annotated code "join_map")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(Str => List(Str)), ((Str => List(Str)) -> List(b)) -> List(b)")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But I need it to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(Str => List(Str)), ((a -> c) -> c) -> List(b)")
			(line-break)
			(indent 1)
			(text "  where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenSquare,LowerIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenSquare,LowerIdent,CloseSquare,
KwExpect,LowerIdent,NoSpaceOpenRound,OpenSquare,StringStart,StringPart,StringEnd,CloseSquare,Comma,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,CloseRound,OpPizza,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "apply"))
			(e-lambda
				(args
					(p-ident (raw "lines"))
					(p-ident (raw "functions")))
				(e-method-call (method ".join_map")
					(receiver
						(e-ident (raw "lines")))
					(args
						(e-lambda
							(args
								(p-ident (raw "line")))
							(e-method-call (method ".join_map")
								(receiver
									(e-ident (raw "functions")))
								(args
									(e-lambda
										(args
											(p-ident (raw "f")))
										(e-apply
											(e-ident (raw "f"))
											(e-ident (raw "line")))))))))))
		(s-decl
			(p-ident (raw "pure_one"))
			(e-lambda
				(args
					(p-ident (raw "line")))
				(e-list
					(e-ident (raw "line")))))
		(s-type-anno (name "shout!")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "List"))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "shout!"))
			(e-lambda
				(args
					(p-ident (raw "line")))
				(e-list
					(e-ident (raw "line")))))
		(s-expect
			(e-arrow-call
				(e-apply
					(e-ident (raw "apply"))
					(e-list
						(e-string
							(e-string-part (raw "a"))))
					(e-list
						(e-ident (raw "pure_one"))
						(e-ident (raw "shout!"))))
				(e-apply
					(e-ident (raw "List.is_eq"))
					(e-list
						(e-string
							(e-string-part (raw "a")))
						(e-string
							(e-string-part (raw "a")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "apply"))
		(e-lambda
			(args
				(p-assign (ident "lines"))
				(p-assign (ident "functions")))
			(e-dispatch-call (method "join_map") (constraint-fn-var 267)
				(receiver
					(e-lookup-local
						(p-assign (ident "lines"))))
				(args
					(e-closure
						(captures
							(capture (ident "functions")))
						(e-lambda
							(args
								(p-assign (ident "line")))
							(e-runtime-error (tag "erroneous_value_expr"))))))))
	(d-let
		(p-assign (ident "pure_one"))
		(e-lambda
			(args
				(p-assign (ident "line")))
			(e-list
				(elems
					(e-lookup-local
						(p-assign (ident "line")))))))
	(d-let
		(p-assign (ident "shout!"))
		(e-lambda
			(args
				(p-assign (ident "line")))
			(e-list
				(elems
					(e-lookup-local
						(p-assign (ident "line"))))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Str") (builtin))))))
	(s-expect
		(e-call (constraint-fn-var 364)
			(e-lookup-external
				(builtin))
			(e-call (constraint-fn-var 322)
				(e-lookup-local
					(p-assign (ident "apply")))
				(e-list
					(elems
						(e-string
							(e-literal (string "a")))))
				(e-list
					(elems
						(e-lookup-local
							(p-assign (ident "pure_one")))
						(e-lookup-local
							(p-assign (ident "shout!"))))))
			(e-list
				(elems
					(e-string
						(e-literal (string "a")))
					(e-string
						(e-literal (string "a"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "c, d -> e where [c.join_map : c, (g -> h) -> e, d.join_map : d, ((g -> i) -> i) -> h]"))
		(patt (type "c -> List(c)"))
		(patt (type "Str => List(Str)")))
	(expressions
		(expr (type "c, d -> e where [c.join_map : c, (g -> h) -> e, d.join_map : d, ((g -> i) -> i) -> h]"))
		(expr (type "c -> List(c)"))
		(expr (type "Str => List(Str)"))))
~~~
