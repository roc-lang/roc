# META
~~~ini
description=A nominal pattern whose backing pattern does not match the declared backing reports one mismatch
type=snippet
~~~
# SOURCE
~~~roc
Pt := { depth : U8, n : U64 }.{
	f : Pt -> U8

	f = |s| match s {
		Pt([a]) => a
		_ => 0
	}
}
~~~
# EXPECTED
TYPE MISMATCH - issue_10788_nominal_pattern_backing_mismatch.md:4:10:4:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 4 10) (end 7 3))
		(headline
			(reflow "The first pattern in this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "is incompatible."))
		(document
			(source-underlines
				(display (file "issue_10788_nominal_pattern_backing_mismatch.md") (start 4 10) (end 7 3) (annotation dim) (line-text "\tf = |s| match s {\n\t\tPt([a]) => a\n\t\t_ => 0\n\t}"))
				(underline (start 5 3) (end 5 10) (annotation error)))
			(line-break)
			(reflow "The first pattern is trying to match:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Pt(List(_b)), ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the expression between the")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "parenthesis has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Pt")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "These can never match! Either the pattern or expression has a problem."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,OpenSquare,LowerIdent,CloseSquare,CloseRound,OpFatArrow,LowerIdent,
Underscore,OpFatArrow,Int,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Pt")
				(args))
			(ty-record
				(anno-record-field (name "depth")
					(ty (name "U8")))
				(anno-record-field (name "n")
					(ty (name "U64"))))
			(associated
				(s-type-anno (name "f")
					(ty-fn
						(ty (name "Pt"))
						(ty (name "U8"))))
				(s-decl
					(p-ident (raw "f"))
					(e-lambda
						(args
							(p-ident (raw "s")))
						(e-match
							(e-ident (raw "s"))
							(branches
								(branch
									(p-tag (raw "Pt")
										(p-list
											(p-ident (raw "a"))))
									(e-ident (raw "a")))
								(branch
									(p-underscore)
									(e-int (raw "0")))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "issue_10788_nominal_pattern_backing_mismatch.Pt.f"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Pt") (local))
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Pt"))
		(ty-record
			(field (field "depth")
				(ty-lookup (name "U8") (builtin)))
			(field (field "n")
				(ty-lookup (name "U64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Pt -> U8")))
	(type_decls
		(nominal (type "Pt")
			(ty-header (name "Pt"))))
	(expressions
		(expr (type "Pt -> U8"))))
~~~
