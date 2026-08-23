# META
~~~ini
description=A nominal constructor built from a fresh record literal is accepted in the same inline-lambda position
type=snippet
~~~
# SOURCE
~~~roc
Sk := { depth : U8, n : U64 }.{
	f : List(Sk) -> List(Sk)

	f = |xs| List.map(xs, |x| Sk.{ depth: 1, n: x.n })
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,OpBar,LowerIdent,OpBar,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Sk")
				(args))
			(ty-record
				(anno-record-field (name "depth")
					(ty (name "U8")))
				(anno-record-field (name "n")
					(ty (name "U64"))))
			(associated
				(s-type-anno (name "f")
					(ty-fn
						(ty-apply
							(ty (name "List"))
							(ty (name "Sk")))
						(ty-apply
							(ty (name "List"))
							(ty (name "Sk")))))
				(s-decl
					(p-ident (raw "f"))
					(e-lambda
						(args
							(p-ident (raw "xs")))
						(e-apply
							(e-ident (raw "List.map"))
							(e-ident (raw "xs"))
							(e-lambda
								(args
									(p-ident (raw "x")))
								(e-nominal-record
									(mapper (e-tag (raw "Sk")))
									(backing (e-record
											(field (field "depth")
												(e-int (raw "1")))
											(field (field "n")
												(e-field-access
													(receiver
														(e-ident (raw "x")))
													(segment (mode "required") (field "n")))))))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "issue_10788_nominal_record_construction_in_lambda.Sk.f"))
		(e-lambda
			(args
				(p-assign (ident "xs")))
			(e-call (constraint-fn-var 284)
				(e-lookup-external
					(builtin))
				(e-lookup-local
					(p-assign (ident "xs")))
				(e-lambda
					(args
						(p-assign (ident "x")))
					(e-nominal (nominal "Sk")
						(e-record
							(fields
								(field (name "depth")
									(e-num (value "1")))
								(field (name "n")
									(e-field-access
										(receiver
											(e-lookup-local
												(p-assign (ident "x"))))
										(segments
											(segment (name "n") (mode "required")))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Sk") (local)))
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Sk") (local))))))
	(s-nominal-decl
		(ty-header (name "Sk"))
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
		(patt (type "List(Sk) -> List(Sk)")))
	(type_decls
		(nominal (type "Sk")
			(ty-header (name "Sk"))))
	(expressions
		(expr (type "List(Sk) -> List(Sk)"))))
~~~
