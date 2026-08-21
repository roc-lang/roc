# META
~~~ini
description=Issue #10699: a rejected field access reports once and leaves the record it reads from solved, so lowering never sees an erroneous type
type=file
~~~
# SOURCE
~~~roc
record : { alpha : U64, beta : U64 }
record = { alpha: 1, beta: 2 }

sum = record.alpha + record.beta

missing = record.gamma
~~~
# EXPECTED
TYPE MISMATCH - record_access_missing_field_keeps_record_type.md:6:17:6:23
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 6 17) (end 6 23))
		(headline
			(reflow "This record does not have a")
			(reflow " ")
			(annotated code "gamma")
			(reflow " ")
			(reflow "field."))
		(document
			(source-region (file "record_access_missing_field_keeps_record_type.md") (start 6 17) (end 6 23) (annotation error) (line-text "missing = record.gamma"))
			(line-break)
			(reflow "This is often due to a typo. The most similar fields are:")
			(line-break)
			(line-break)
			(text "    - ")
			(annotated code "beta")
			(line-break)
			(text "    - ")
			(annotated code "alpha")
			(line-break)
			(line-break)
			(reflow "So maybe ")
			(annotated code "gamma")
			(reflow " should be ")
			(annotated code "beta")
			(text "?"))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,OpPlus,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "record")
			(ty-record
				(anno-record-field (name "alpha")
					(ty (name "U64")))
				(anno-record-field (name "beta")
					(ty (name "U64")))))
		(s-decl
			(p-ident (raw "record"))
			(e-record
				(field (field "alpha")
					(e-int (raw "1")))
				(field (field "beta")
					(e-int (raw "2")))))
		(s-decl
			(p-ident (raw "sum"))
			(e-binop (op "+")
				(e-field-access
					(receiver
						(e-ident (raw "record")))
					(segment (mode "required") (field "alpha")))
				(e-field-access
					(receiver
						(e-ident (raw "record")))
					(segment (mode "required") (field "beta")))))
		(s-decl
			(p-ident (raw "missing"))
			(e-field-access
				(receiver
					(e-ident (raw "record")))
				(segment (mode "required") (field "gamma"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "record"))
		(e-record
			(fields
				(field (name "alpha")
					(e-num (value "1")))
				(field (name "beta")
					(e-num (value "2")))))
		(annotation
			(ty-record
				(field (field "alpha")
					(ty-lookup (name "U64") (builtin)))
				(field (field "beta")
					(ty-lookup (name "U64") (builtin))))))
	(d-let
		(p-assign (ident "sum"))
		(e-dispatch-call (method "plus") (constraint-fn-var 265)
			(receiver
				(e-field-access
					(receiver
						(e-lookup-local
							(p-assign (ident "record"))))
					(segments
						(segment (name "alpha") (mode "required")))))
			(args
				(e-field-access
					(receiver
						(e-lookup-local
							(p-assign (ident "record"))))
					(segments
						(segment (name "beta") (mode "required")))))))
	(d-let
		(p-assign (ident "missing"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ alpha: U64, beta: U64 }"))
		(patt (type "U64"))
		(patt (type "Error")))
	(expressions
		(expr (type "{ alpha: U64, beta: U64 }"))
		(expr (type "U64"))
		(expr (type "Error"))))
~~~
