# META
~~~ini
description=A destructured binder of a determined field kind returned at another type is reported at the result, as a field read would be
type=snippet
~~~
# SOURCE
~~~roc
first_tag : { name : Str, tags : List(Str) } -> Str
first_tag = |item| {
	{ tags, .. } = item
	tags
}
~~~
# EXPECTED
TYPE MISMATCH - record_destructure_binder_result_mismatch.md:4:2:4:6
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 4 2) (end 4 6))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "record_destructure_binder_result_mismatch.md") (start 4 2) (end 4 6) (annotation error) (line-text "\ttags"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(Str)")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
OpenCurly,LowerIdent,Comma,DoubleDot,CloseCurly,OpAssign,LowerIdent,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "first_tag")
			(ty-fn
				(ty-record
					(anno-record-field (name "name")
						(ty (name "Str")))
					(anno-record-field (name "tags")
						(ty-apply
							(ty (name "List"))
							(ty (name "Str")))))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "first_tag"))
			(e-lambda
				(args
					(p-ident (raw "item")))
				(e-block
					(statements
						(s-decl
							(p-record
								(field (name "tags") (rest false))
								(field (rest true)))
							(e-ident (raw "item")))
						(e-ident (raw "tags"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "first_tag"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "name")
						(ty-lookup (name "Str") (builtin)))
					(field (field "tags")
						(ty-apply (name "List") (builtin)
							(ty-lookup (name "Str") (builtin)))))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ name: Str, tags: List(Str) } -> Str")))
	(expressions
		(expr (type "{ name: Str, tags: List(Str) } -> Str"))))
~~~
