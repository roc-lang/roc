# META
~~~ini
description=A generalized interpolation used at Str rejects a non-Str part at that use, leaving other uses intact.
type=snippet
~~~
# SOURCE
~~~roc
greet = |name| "hi ${name}"

ok : Str
ok = greet("there")

bad : Str
bad = greet(42.U64)
~~~
# EXPECTED
TYPE MISMATCH - generic_interpolation_use_rejects_part.md:7:7:7:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 7 13) (end 7 19))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument being passed to this function has the wrong type."))
		(document
			(source-underlines
				(display (file "generic_interpolation_use_rejects_part.md") (start 7 7) (end 7 20) (annotation dim) (line-text "bad = greet(42.U64)"))
				(underline (start 7 13) (end 7 19) (annotation error)))
			(line-break)
			(reflow "This argument has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U64")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But")
			(reflow " ")
			(annotated code "greet")
			(reflow " ")
			(reflow "needs the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Convert it to a")
			(reflow " ")
			(annotated code "Str")
			(reflow " ")
			(reflow "with")
			(reflow " ")
			(annotated code ".to_str()")
			(reflow "."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "greet"))
			(e-lambda
				(args
					(p-ident (raw "name")))
				(e-string
					(e-string-part (raw "hi "))
					(e-ident (raw "name"))
					(e-string-part (raw "")))))
		(s-type-anno (name "ok")
			(ty (name "Str")))
		(s-decl
			(p-ident (raw "ok"))
			(e-apply
				(e-ident (raw "greet"))
				(e-string
					(e-string-part (raw "there")))))
		(s-type-anno (name "bad")
			(ty (name "Str")))
		(s-decl
			(p-ident (raw "bad"))
			(e-apply
				(e-ident (raw "greet"))
				(e-typed-int (raw "42") (type "U64"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "greet"))
		(e-lambda
			(args
				(p-assign (ident "name")))
			(e-block
				(s-let
					(p-assign (ident "#interp_0"))
					(e-lookup-local
						(p-assign (ident "name"))))
				(e-interpolation (constraint-fn-var 249) (dispatcher-var 10)
					(first
						(e-literal (string "hi ")))
					(parts
						(e-lookup-local
							(p-assign (ident "#interp_0")))
						(e-literal (string "")))))))
	(d-let
		(p-assign (ident "ok"))
		(e-call (constraint-fn-var 271)
			(e-lookup-local
				(p-assign (ident "greet")))
			(e-string
				(e-literal (string "there"))))
		(annotation
			(ty-lookup (name "Str") (builtin))))
	(d-let
		(p-assign (ident "bad"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "Str") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "item -> a where [a.from_interpolation : List(Str) -> Try(Iter(item) -> a, [InvalidInterpolation(Str)])]"))
		(patt (type "Str"))
		(patt (type "Str")))
	(expressions
		(expr (type "item -> a where [a.from_interpolation : List(Str) -> Try(Iter(item) -> a, [InvalidInterpolation(Str)])]"))
		(expr (type "Str"))
		(expr (type "Str"))))
~~~
