# META
~~~ini
description=Same-receiver interpolations share a callable relation but retain occurrence-specific part checks.
type=file
~~~
# SOURCE
~~~roc
Rendered := [Rendered].{
    from_interpolation : Str, Iter((U64, Str)) -> Rendered
    from_interpolation = |_, _| Rendered.Rendered
}

build = |good, bad| ["${good}", "${bad}"]

main : List(Rendered)
main = build(1.U64, "not a number")
~~~
# EXPECTED
TYPE MISMATCH - every_interpolation_occurrence_checks_parts.md:9:21:9:35
# PROBLEMS
── ✗ type mismatch ───────── every_interpolation_occurrence_checks_parts.md:9:21

This string literal is being used where a non-string type is needed.

main = build(1.U64, "not a number")
                    ^^^^^^^^^^^^^^

The type was determined to be:

    U64

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,Comma,Underscore,OpBar,UpperIdent,NoSpaceDotUpperIdent,
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
						(ty (name "Str"))
						(ty-apply
							(ty (name "Iter"))
							(ty-tuple
								(ty (name "U64"))
								(ty (name "Str"))))
						(ty (name "Rendered"))))
				(s-decl
					(p-ident (raw "from_interpolation"))
					(e-lambda
						(args
							(p-underscore)
							(p-underscore))
						(e-tag (raw "Rendered.Rendered"))))))
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
	from_interpolation : Str, Iter((U64, Str)) -> Rendered
	from_interpolation = |_, _| Rendered.Rendered
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
				(p-underscore)
				(p-underscore))
			(e-nominal (nominal "Rendered")
				(e-tag (name "Rendered"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Iter") (builtin)
					(ty-tuple
						(ty-lookup (name "U64") (builtin))
						(ty-lookup (name "Str") (builtin))))
				(ty-lookup (name "Rendered") (local)))))
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
						(e-interpolation (constraint-fn-var 305) (dispatcher-var 30)
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
						(e-interpolation (constraint-fn-var 323) (dispatcher-var 38)
							(first
								(e-literal (string "")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_1")))
								(e-literal (string "")))))))))
	(d-let
		(p-assign (ident "main"))
		(e-call (constraint-fn-var 354)
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
		(patt (type "Str, Iter((U64, Str)) -> Rendered"))
		(patt (type "_arg, _arg2 -> List(a) where [a.from_interpolation : Str, Iter((b, Str)) -> a, a.from_interpolation : Str, Iter((b, Str)) -> a]"))
		(patt (type "List(Rendered)")))
	(type_decls
		(nominal (type "Rendered")
			(ty-header (name "Rendered"))))
	(expressions
		(expr (type "Str, Iter((U64, Str)) -> Rendered"))
		(expr (type "_arg, _arg2 -> List(a) where [a.from_interpolation : Str, Iter((b, Str)) -> a, a.from_interpolation : Str, Iter((b, Str)) -> a]"))
		(expr (type "List(Rendered)"))))
~~~
