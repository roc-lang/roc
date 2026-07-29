# META
~~~ini
description=Chained optional field accesses collapse into one flat Try per chain
type=snippet
~~~
# SOURCE
~~~roc
Inner : { c ?: U8 }
Outer : { b ?: Inner }

chain : Outer -> Try(U8, [MissingField])
chain = |o| o.?b.?c

mixed : { b ?: { c : U8 } } -> Try(U8, [MissingField])
mixed = |o| o.?b.c

with_default : Outer -> U8
with_default = |o| o.?b.?c ?? 0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,OpenCurly,LowerIdent,OpQuestion,OpColon,UpperIdent,CloseCurly,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpQuestion,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotQuestionLowerIdent,NoSpaceDotQuestionLowerIdent,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpQuestion,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,CloseCurly,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotQuestionLowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotQuestionLowerIdent,NoSpaceDotQuestionLowerIdent,OpDoubleQuestion,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Inner")
				(args))
			(ty-record
				(anno-record-field (name "c") (optional true)
					(ty (name "U8")))))
		(s-type-decl
			(header (name "Outer")
				(args))
			(ty-record
				(anno-record-field (name "b") (optional true)
					(ty (name "Inner")))))
		(s-type-anno (name "chain")
			(ty-fn
				(ty (name "Outer"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "U8"))
					(ty-tag-union
						(tags
							(ty (name "MissingField")))))))
		(s-decl
			(p-ident (raw "chain"))
			(e-lambda
				(args
					(p-ident (raw "o")))
				(e-field-access
					(receiver
						(e-ident (raw "o")))
					(segment (mode "optional") (field "b"))
					(segment (mode "optional") (field "c")))))
		(s-type-anno (name "mixed")
			(ty-fn
				(ty-record
					(anno-record-field (name "b") (optional true)
						(ty-record
							(anno-record-field (name "c")
								(ty (name "U8"))))))
				(ty-apply
					(ty (name "Try"))
					(ty (name "U8"))
					(ty-tag-union
						(tags
							(ty (name "MissingField")))))))
		(s-decl
			(p-ident (raw "mixed"))
			(e-lambda
				(args
					(p-ident (raw "o")))
				(e-field-access
					(receiver
						(e-ident (raw "o")))
					(segment (mode "optional") (field "b"))
					(segment (mode "required") (field "c")))))
		(s-type-anno (name "with_default")
			(ty-fn
				(ty (name "Outer"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "with_default"))
			(e-lambda
				(args
					(p-ident (raw "o")))
				(e-binop (op "??")
					(e-field-access
						(receiver
							(e-ident (raw "o")))
						(segment (mode "optional") (field "b"))
						(segment (mode "optional") (field "c")))
					(e-int (raw "0")))))))
~~~
# FORMATTED
~~~roc
Inner : { c ?: U8 }

Outer : { b ?: Inner }

chain : Outer -> Try(U8, [MissingField])
chain = |o| o.?b.?c

mixed : { b ?: { c : U8 } } -> Try(U8, [MissingField])
mixed = |o| o.?b.c

with_default : Outer -> U8
with_default = |o| o.?b.?c ?? 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "chain"))
		(e-lambda
			(args
				(p-assign (ident "o")))
			(e-field-access
				(receiver
					(e-lookup-local
						(p-assign (ident "o"))))
				(segments
					(segment (name "b") (mode "optional"))
					(segment (name "c") (mode "optional")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Outer") (local))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "U8") (builtin))
					(ty-tag-union
						(ty-tag-name (name "MissingField")))))))
	(d-let
		(p-assign (ident "mixed"))
		(e-lambda
			(args
				(p-assign (ident "o")))
			(e-field-access
				(receiver
					(e-lookup-local
						(p-assign (ident "o"))))
				(segments
					(segment (name "b") (mode "optional"))
					(segment (name "c") (mode "required")))))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "b") (optional true)
						(ty-record
							(field (field "c")
								(ty-lookup (name "U8") (builtin))))))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "U8") (builtin))
					(ty-tag-union
						(ty-tag-name (name "MissingField")))))))
	(d-let
		(p-assign (ident "with_default"))
		(e-lambda
			(args
				(p-assign (ident "o")))
			(e-match
				(match
					(cond
						(e-field-access
							(receiver
								(e-lookup-local
									(p-assign (ident "o"))))
							(segments
								(segment (name "b") (mode "optional"))
								(segment (name "c") (mode "optional")))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal-external (builtin)
										(p-applied-tag))))
							(value
								(e-lookup-local
									(p-assign (ident "#ok")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal-external (builtin)
										(p-applied-tag))))
							(value
								(e-num (value "0"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Outer") (local))
				(ty-lookup (name "U8") (builtin)))))
	(s-alias-decl
		(ty-header (name "Inner"))
		(ty-record
			(field (field "c") (optional true)
				(ty-lookup (name "U8") (builtin)))))
	(s-alias-decl
		(ty-header (name "Outer"))
		(ty-record
			(field (field "b") (optional true)
				(ty-lookup (name "Inner") (local))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Outer -> Try(U8, [MissingField])"))
		(patt (type "{ b ?: { c: U8 } } -> Try(U8, [MissingField])"))
		(patt (type "Outer -> U8")))
	(type_decls
		(alias (type "Inner")
			(ty-header (name "Inner")))
		(alias (type "Outer")
			(ty-header (name "Outer"))))
	(expressions
		(expr (type "Outer -> Try(U8, [MissingField])"))
		(expr (type "{ b ?: { c: U8 } } -> Try(U8, [MissingField])"))
		(expr (type "Outer -> U8"))))
~~~
