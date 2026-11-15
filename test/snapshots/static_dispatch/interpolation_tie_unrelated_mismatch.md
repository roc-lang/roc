# META
~~~ini
description=A mismatch after a use of a generalized interpolation, in a relation that does not relate that use, is its own report even though its types reach the interpolation's item type
type=file
~~~
# SOURCE
~~~roc
describe = |person| "${person.name} is ${person.age}"

label : { name : Str, age : Str } -> Str
label = |r| {
	s = describe(r)
	x : U64
	x = r.age
	Str.concat(s, x.to_str())
}
~~~
# EXPECTED
TYPE MISMATCH - interpolation_tie_unrelated_mismatch.md:7:6:7:11
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 7 6) (end 7 11))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "interpolation_tie_unrelated_mismatch.md") (start 7 6) (end 7 11) (annotation error) (line-text "\tx = r.age"))
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
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U64")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "describe"))
			(e-lambda
				(args
					(p-ident (raw "person")))
				(e-string
					(e-string-part (raw ""))
					(e-field-access
						(receiver
							(e-ident (raw "person")))
						(segment (mode "required") (field "name")))
					(e-string-part (raw " is "))
					(e-field-access
						(receiver
							(e-ident (raw "person")))
						(segment (mode "required") (field "age")))
					(e-string-part (raw "")))))
		(s-type-anno (name "label")
			(ty-fn
				(ty-record
					(anno-record-field (name "name")
						(ty (name "Str")))
					(anno-record-field (name "age")
						(ty (name "Str"))))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "label"))
			(e-lambda
				(args
					(p-ident (raw "r")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "s"))
							(e-apply
								(e-ident (raw "describe"))
								(e-ident (raw "r"))))
						(s-type-anno (name "x")
							(ty (name "U64")))
						(s-decl
							(p-ident (raw "x"))
							(e-field-access
								(receiver
									(e-ident (raw "r")))
								(segment (mode "required") (field "age"))))
						(e-apply
							(e-ident (raw "Str.concat"))
							(e-ident (raw "s"))
							(e-method-call (method ".to_str")
								(receiver
									(e-ident (raw "x")))
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
		(p-assign (ident "describe"))
		(e-lambda
			(args
				(p-assign (ident "person")))
			(e-block
				(s-let
					(p-assign (ident "#interp_0"))
					(e-field-access
						(receiver
							(e-lookup-local
								(p-assign (ident "person"))))
						(segments
							(segment (name "name") (mode "required")))))
				(s-let
					(p-assign (ident "#interp_1"))
					(e-field-access
						(receiver
							(e-lookup-local
								(p-assign (ident "person"))))
						(segments
							(segment (name "age") (mode "required")))))
				(e-interpolation (constraint-fn-var 287) (dispatcher-var 18)
					(first
						(e-literal (string "")))
					(parts
						(e-lookup-local
							(p-assign (ident "#interp_0")))
						(e-literal (string " is "))
						(e-lookup-local
							(p-assign (ident "#interp_1")))
						(e-literal (string "")))))))
	(d-let
		(p-assign (ident "label"))
		(e-lambda
			(args
				(p-assign (ident "r")))
			(e-block
				(s-let
					(p-assign (ident "s"))
					(e-call (constraint-fn-var 328)
						(e-lookup-local
							(p-assign (ident "describe")))
						(e-lookup-local
							(p-assign (ident "r")))))
				(s-let
					(p-assign (ident "x"))
					(e-runtime-error (tag "erroneous_value_expr")))
				(e-call (constraint-fn-var 348)
					(e-lookup-external
						(builtin))
					(e-lookup-local
						(p-assign (ident "s")))
					(e-dispatch-call (method "to_str") (constraint-fn-var 340)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args)))))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "name")
						(ty-lookup (name "Str") (builtin)))
					(field (field "age")
						(ty-lookup (name "Str") (builtin))))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ age: item, name: item, .. } -> a where [a.from_interpolation : List(Str) -> Try(Iter(item) -> a, [InvalidInterpolation(Str)])]"))
		(patt (type "{ age: Str, name: Str } -> Str")))
	(expressions
		(expr (type "{ age: item, name: item, .. } -> a where [a.from_interpolation : List(Str) -> Try(Iter(item) -> a, [InvalidInterpolation(Str)])]"))
		(expr (type "{ age: Str, name: Str } -> Str"))))
~~~
