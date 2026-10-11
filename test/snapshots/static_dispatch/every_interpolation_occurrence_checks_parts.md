# META
~~~ini
description=Same-receiver interpolations share a callable relation but retain occurrence-specific part checks.
type=file
~~~
# SOURCE
~~~roc
Rendered := [Rendered].{
    from_interpolation : List(Str) -> Try((List(U64) -> Rendered), [InvalidInterpolation(Str)])
    from_interpolation = |_| Ok(|_| Rendered.Rendered)
}

build = |good, bad| ["${good}", "${bad}"]

main : List(Rendered)
main = build(1.U64, "not a number")
~~~
# EXPECTED
TYPE MISMATCH - every_interpolation_occurrence_checks_parts.md:9:21:9:35
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 9 21) (end 9 35))
		(headline
			(reflow "This string literal is being used where a non-string type is needed."))
		(document
			(source-region (file "every_interpolation_occurrence_checks_parts.md") (start 9 21) (end 9 35) (annotation error) (line-text "main = build(1.U64, \"not a number\")"))
			(line-break)
			(reflow "The type was determined to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U64")
			(annotation-end))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,CloseRound,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,OpBar,Underscore,OpBar,UpperIdent,NoSpaceDotUpperIdent,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenSquare,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,Comma,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,Comma,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Rendered")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Rendered"))))
			(associated
				(s-type-anno (name "from_interpolation")
					(ty-fn
						(ty-apply
							(ty (name "List"))
							(ty (name "Str")))
						(ty-apply
							(ty (name "Try"))
							(ty-fn
								(ty-apply
									(ty (name "List"))
									(ty (name "U64")))
								(ty (name "Rendered")))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "InvalidInterpolation"))
										(ty (name "Str"))))))))
				(s-decl
					(p-ident (raw "from_interpolation"))
					(e-lambda
						(args
							(p-underscore))
						(e-apply
							(e-tag (raw "Ok"))
							(e-lambda
								(args
									(p-underscore))
								(e-tag (raw "Rendered.Rendered"))))))))
		(s-decl
			(p-ident (raw "build"))
			(e-lambda
				(args
					(p-ident (raw "good"))
					(p-ident (raw "bad")))
				(e-list
					(e-string
						(e-string-part (raw ""))
						(e-ident (raw "good"))
						(e-string-part (raw "")))
					(e-string
						(e-string-part (raw ""))
						(e-ident (raw "bad"))
						(e-string-part (raw ""))))))
		(s-type-anno (name "main")
			(ty-apply
				(ty (name "List"))
				(ty (name "Rendered"))))
		(s-decl
			(p-ident (raw "main"))
			(e-apply
				(e-ident (raw "build"))
				(e-typed-int (raw "1") (type "U64"))
				(e-string
					(e-string-part (raw "not a number")))))))
~~~
# FORMATTED
~~~roc
Rendered := [Rendered].{
	from_interpolation : List(Str) -> Try((List(U64) -> Rendered), [InvalidInterpolation(Str)])
	from_interpolation = |_| Ok(|_| Rendered.Rendered)
}

build = |good, bad| ["${good}", "${bad}"]

main : List(Rendered)
main = build(1.U64, "not a number")
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "every_interpolation_occurrence_checks_parts.Rendered.from_interpolation"))
		(e-lambda
			(args
				(p-underscore))
			(e-tag (name "Ok")
				(args
					(e-lambda
						(args
							(p-underscore))
						(e-nominal (nominal "Rendered")
							(e-tag (name "Rendered")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Str") (builtin)))
				(ty-apply (name "Try") (builtin)
					(ty-parens
						(ty-fn (effectful false)
							(ty-apply (name "List") (builtin)
								(ty-lookup (name "U64") (builtin)))
							(ty-lookup (name "Rendered") (local))))
					(ty-tag-union
						(ty-tag-name (name "InvalidInterpolation")
							(ty-lookup (name "Str") (builtin))))))))
	(d-let
		(p-assign (ident "build"))
		(e-lambda
			(args
				(p-assign (ident "good"))
				(p-assign (ident "bad")))
			(e-list
				(elems
					(e-block
						(s-let
							(p-assign (ident "#interp_0"))
							(e-lookup-local
								(p-assign (ident "good"))))
						(e-interpolation (constraint-fn-var 327) (dispatcher-var 39)
							(first
								(e-literal (string "")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_0")))
								(e-literal (string "")))))
					(e-block
						(s-let
							(p-assign (ident "#interp_1"))
							(e-lookup-local
								(p-assign (ident "bad"))))
						(e-interpolation (constraint-fn-var 341) (dispatcher-var 47)
							(first
								(e-literal (string "")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_1")))
								(e-literal (string "")))))))))
	(d-let
		(p-assign (ident "main"))
		(e-call (constraint-fn-var 362)
			(e-lookup-local
				(p-assign (ident "build")))
			(e-typed-int (value "1") (type "U64"))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "Rendered") (local)))))
	(s-nominal-decl
		(ty-header (name "Rendered"))
		(ty-tag-union
			(ty-tag-name (name "Rendered")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Str) -> Try(List(U64) -> Rendered, [InvalidInterpolation(Str)])"))
		(patt (type "a, a -> List(b) where [b.from_interpolation : List(Str) -> Try(List(a) -> b, [InvalidInterpolation(Str)]), b.from_interpolation : List(Str) -> Try(List(a) -> b, [InvalidInterpolation(Str)])]"))
		(patt (type "List(Rendered)")))
	(type_decls
		(nominal (type "Rendered")
			(ty-header (name "Rendered"))))
	(expressions
		(expr (type "List(Str) -> Try(List(U64) -> Rendered, [InvalidInterpolation(Str)])"))
		(expr (type "a, a -> List(b) where [b.from_interpolation : List(Str) -> Try(List(a) -> b, [InvalidInterpolation(Str)]), b.from_interpolation : List(Str) -> Try(List(a) -> b, [InvalidInterpolation(Str)])]"))
		(expr (type "List(Rendered)"))))
~~~
