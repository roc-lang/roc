# META
~~~ini
description=Binders of a pattern rejected while it is checked are erroneous, so uses of them add no reports of their own
type=snippet
~~~
# SOURCE
~~~roc
Cents := [Cents(U64, U64)]

show : Cents -> Str
show = |Cents.Cents(c)| c.to_str()

describe : Cents -> Str
describe = |cents| match cents {
	Cents.Cents(c) => c.to_str()
}
~~~
# EXPECTED
INVALID NOMINAL TAG - rejected_pattern_binder_uses.md:4:9:4:23
INVALID NOMINAL TAG - rejected_pattern_binder_uses.md:8:2:8:16
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Nominal Tag")
		(region (start 4 9) (end 4 23))
		(headline
			(reflow "I'm having trouble with this nominal tag."))
		(document
			(source-region (file "rejected_pattern_binder_uses.md") (start 4 9) (end 4 23) (annotation error) (line-text "show = |Cents.Cents(c)| c.to_str()"))
			(line-break)
			(text "The tag is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Cents(_a)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "But the nominal type needs it to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Cents(U64, U64)")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Invalid Nominal Tag")
		(region (start 8 2) (end 8 16))
		(headline
			(reflow "I'm having trouble with this nominal tag."))
		(document
			(source-region (file "rejected_pattern_binder_uses.md") (start 8 2) (end 8 16) (annotation error) (line-text "\tCents.Cents(c) => c.to_str()"))
			(line-break)
			(text "The tag is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Cents(_a)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "But the nominal type needs it to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Cents(U64, U64)")
			(annotation-end))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Cents")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Cents"))
						(ty (name "U64"))
						(ty (name "U64"))))))
		(s-type-anno (name "show")
			(ty-fn
				(ty (name "Cents"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "show"))
			(e-lambda
				(args
					(p-tag (raw ".Cents")
						(p-ident (raw "c"))))
				(e-method-call (method ".to_str")
					(receiver
						(e-ident (raw "c")))
					(args))))
		(s-type-anno (name "describe")
			(ty-fn
				(ty (name "Cents"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "describe"))
			(e-lambda
				(args
					(p-ident (raw "cents")))
				(e-match
					(e-ident (raw "cents"))
					(branches
						(branch
							(p-tag (raw ".Cents")
								(p-ident (raw "c")))
							(e-method-call (method ".to_str")
								(receiver
									(e-ident (raw "c")))
								(args)))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "show"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Cents") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "describe"))
		(e-lambda
			(args
				(p-assign (ident "cents")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Cents") (local))
				(ty-lookup (name "Str") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Cents"))
		(ty-tag-union
			(ty-tag-name (name "Cents")
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "U64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Cents -> Str"))
		(patt (type "Cents -> Str")))
	(type_decls
		(nominal (type "Cents")
			(ty-header (name "Cents"))))
	(expressions
		(expr (type "Cents -> Str"))
		(expr (type "Cents -> Str"))))
~~~
