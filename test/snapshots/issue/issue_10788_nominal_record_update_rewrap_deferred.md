# META
~~~ini
description=A nominal constructor rejects a nominal record backing that only lifts after the relation runs
type=snippet
~~~
# SOURCE
~~~roc
Sc := { depth : U8, n : U64 }.{
	f : List(Sc) -> List(Sc)

	f = |xs| List.map(xs, |x| Sc.{ ..x, depth: 1 })
}
~~~
# EXPECTED
INVALID NOMINAL RECORD - issue_10788_nominal_record_update_rewrap_deferred.md:4:31:4:48
# PROBLEMS
── ✗ invalid nominal record ─ issue_10788_nominal_record_update_rewrap_deferred.md:4:31

I'm having trouble with this nominal type that wraps a record.

f = |xs| List.map(xs, |x| Sc.{ ..x, depth: 1 })
                             ^^^^^^^^^^^^^^^^^

The record I found is:

    Sc

But the nominal type expects:

    { depth: U8, n: U64 }

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,OpBar,LowerIdent,OpBar,UpperIdent,Dot,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Sc")
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
							(ty (name "Sc")))
						(ty-apply
							(ty (name "List"))
							(ty (name "Sc")))))
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
									(mapper (e-tag (raw "Sc")))
									(backing (e-record
											(ext
												(e-ident (raw "x")))
											(field (field "depth")
												(e-int (raw "1"))))))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "issue_10788_nominal_record_update_rewrap_deferred.Sc.f"))
		(e-lambda
			(args
				(p-assign (ident "xs")))
			(e-call (constraint-fn-var 279)
				(e-lookup-external
					(builtin))
				(e-lookup-local
					(p-assign (ident "xs")))
				(e-lambda
					(args
						(p-assign (ident "x")))
					(e-runtime-error (tag "erroneous_value_expr")))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Sc") (local)))
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Sc") (local))))))
	(s-nominal-decl
		(ty-header (name "Sc"))
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
		(patt (type "List(Sc) -> List(Sc)")))
	(type_decls
		(nominal (type "Sc")
			(ty-header (name "Sc"))))
	(expressions
		(expr (type "List(Sc) -> List(Sc)"))))
~~~
