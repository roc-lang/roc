# META
~~~ini
description=An empty record literal absorbs a nominal record's defaulted and optional fields wherever a literal that omits fields does
type=snippet
~~~
# SOURCE
~~~roc
# repro for https://github.com/roc-lang/roc/issues/11271
Config := { count : U8 ?? 10, other : U8 ?? 20 }

Mixed := { label ?: Str, count : U8 ?? 7 }

id : a -> a
id = |x| x

total : Config -> U8
total = |config| config.count

# A literal that omits `other` absorbs its default, and so must `{}`.
supplied : Config
supplied = id({ count: 1 })

omitted : Config
omitted = id({})

# Same pairing through a builtin that only learns the element type from the
# annotation on the def.
supplied_list : List(Config)
supplied_list = List.repeat({ count: 1 }, 2)

omitted_list : List(Config)
omitted_list = List.repeat({}, 2)

supplied_mapped : List(Config)
supplied_mapped = List.map([1, 2], |_| { count: 1 })

omitted_mapped : List(Config)
omitted_mapped = List.map([1, 2], |_| {})

# An optional (`?:`) sibling does not change the answer.
omitted_mixed : Mixed
omitted_mixed = id({})

expect total({ count: 1 }) == 1
expect total(Config.{}) == 10
expect total({}) == 10
expect omitted.count == 10
expect omitted.other == 20
expect omitted_mixed.count == 7
expect {
	config : Config
	config = {}
	config.count == 10
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,Comma,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,CloseCurly,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpQuestion,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,Comma,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,Int,Comma,Int,CloseSquare,Comma,OpBar,Underscore,OpBar,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,Int,Comma,Int,CloseSquare,Comma,OpBar,Underscore,OpBar,OpenCurly,CloseCurly,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
KwExpect,LowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,CloseRound,OpEquals,Int,
KwExpect,LowerIdent,NoSpaceOpenRound,UpperIdent,Dot,OpenCurly,CloseCurly,CloseRound,OpEquals,Int,
KwExpect,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,OpEquals,Int,
KwExpect,LowerIdent,NoSpaceDotLowerIdent,OpEquals,Int,
KwExpect,LowerIdent,NoSpaceDotLowerIdent,OpEquals,Int,
KwExpect,LowerIdent,NoSpaceDotLowerIdent,OpEquals,Int,
KwExpect,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,CloseCurly,
LowerIdent,NoSpaceDotLowerIdent,OpEquals,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Config")
				(args))
			(ty-record
				(anno-record-field (name "count")
					(ty (name "U8"))
					(default
						(e-int (raw "10"))))
				(anno-record-field (name "other")
					(ty (name "U8"))
					(default
						(e-int (raw "20"))))))
		(s-type-decl
			(header (name "Mixed")
				(args))
			(ty-record
				(anno-record-field (name "label") (optional true)
					(ty (name "Str")))
				(anno-record-field (name "count")
					(ty (name "U8"))
					(default
						(e-int (raw "7"))))))
		(s-type-anno (name "id")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "a"))))
		(s-decl
			(p-ident (raw "id"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-type-anno (name "total")
			(ty-fn
				(ty (name "Config"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "total"))
			(e-lambda
				(args
					(p-ident (raw "config")))
				(e-field-access
					(receiver
						(e-ident (raw "config")))
					(segment (mode "required") (field "count")))))
		(s-type-anno (name "supplied")
			(ty (name "Config")))
		(s-decl
			(p-ident (raw "supplied"))
			(e-apply
				(e-ident (raw "id"))
				(e-record
					(field (field "count")
						(e-int (raw "1"))))))
		(s-type-anno (name "omitted")
			(ty (name "Config")))
		(s-decl
			(p-ident (raw "omitted"))
			(e-apply
				(e-ident (raw "id"))
				(e-record)))
		(s-type-anno (name "supplied_list")
			(ty-apply
				(ty (name "List"))
				(ty (name "Config"))))
		(s-decl
			(p-ident (raw "supplied_list"))
			(e-apply
				(e-ident (raw "List.repeat"))
				(e-record
					(field (field "count")
						(e-int (raw "1"))))
				(e-int (raw "2"))))
		(s-type-anno (name "omitted_list")
			(ty-apply
				(ty (name "List"))
				(ty (name "Config"))))
		(s-decl
			(p-ident (raw "omitted_list"))
			(e-apply
				(e-ident (raw "List.repeat"))
				(e-record)
				(e-int (raw "2"))))
		(s-type-anno (name "supplied_mapped")
			(ty-apply
				(ty (name "List"))
				(ty (name "Config"))))
		(s-decl
			(p-ident (raw "supplied_mapped"))
			(e-apply
				(e-ident (raw "List.map"))
				(e-list
					(e-int (raw "1"))
					(e-int (raw "2")))
				(e-lambda
					(args
						(p-underscore))
					(e-record
						(field (field "count")
							(e-int (raw "1")))))))
		(s-type-anno (name "omitted_mapped")
			(ty-apply
				(ty (name "List"))
				(ty (name "Config"))))
		(s-decl
			(p-ident (raw "omitted_mapped"))
			(e-apply
				(e-ident (raw "List.map"))
				(e-list
					(e-int (raw "1"))
					(e-int (raw "2")))
				(e-lambda
					(args
						(p-underscore))
					(e-record))))
		(s-type-anno (name "omitted_mixed")
			(ty (name "Mixed")))
		(s-decl
			(p-ident (raw "omitted_mixed"))
			(e-apply
				(e-ident (raw "id"))
				(e-record)))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "total"))
					(e-record
						(field (field "count")
							(e-int (raw "1")))))
				(e-int (raw "1"))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "total"))
					(e-nominal-record
						(mapper (e-tag (raw "Config")))
						(backing (e-record))))
				(e-int (raw "10"))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "total"))
					(e-record))
				(e-int (raw "10"))))
		(s-expect
			(e-binop (op "==")
				(e-field-access
					(receiver
						(e-ident (raw "omitted")))
					(segment (mode "required") (field "count")))
				(e-int (raw "10"))))
		(s-expect
			(e-binop (op "==")
				(e-field-access
					(receiver
						(e-ident (raw "omitted")))
					(segment (mode "required") (field "other")))
				(e-int (raw "20"))))
		(s-expect
			(e-binop (op "==")
				(e-field-access
					(receiver
						(e-ident (raw "omitted_mixed")))
					(segment (mode "required") (field "count")))
				(e-int (raw "7"))))
		(s-expect
			(e-block
				(statements
					(s-type-anno (name "config")
						(ty (name "Config")))
					(s-decl
						(p-ident (raw "config"))
						(e-record))
					(e-binop (op "==")
						(e-field-access
							(receiver
								(e-ident (raw "config")))
							(segment (mode "required") (field "count")))
						(e-int (raw "10"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "id"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x"))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
	(d-let
		(p-assign (ident "total"))
		(e-lambda
			(args
				(p-assign (ident "config")))
			(e-field-access
				(receiver
					(e-lookup-local
						(p-assign (ident "config"))))
				(segments
					(segment (name "count") (mode "required")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Config") (local))
				(ty-lookup (name "U8") (builtin)))))
	(d-let
		(p-assign (ident "supplied"))
		(e-call (constraint-fn-var 431)
			(e-lookup-local
				(p-assign (ident "id")))
			(e-record
				(fields
					(field (name "count")
						(e-num (value "1"))))))
		(annotation
			(ty-lookup (name "Config") (local))))
	(d-let
		(p-assign (ident "omitted"))
		(e-call (constraint-fn-var 445)
			(e-lookup-local
				(p-assign (ident "id")))
			(e-empty_record))
		(annotation
			(ty-lookup (name "Config") (local))))
	(d-let
		(p-assign (ident "supplied_list"))
		(e-call (constraint-fn-var 480)
			(e-lookup-external
				(builtin))
			(e-record
				(fields
					(field (name "count")
						(e-num (value "1")))))
			(e-num (value "2")))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "Config") (local)))))
	(d-let
		(p-assign (ident "omitted_list"))
		(e-call (constraint-fn-var 498)
			(e-lookup-external
				(builtin))
			(e-empty_record)
			(e-num (value "2")))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "Config") (local)))))
	(d-let
		(p-assign (ident "supplied_mapped"))
		(e-call (constraint-fn-var 545)
			(e-lookup-external
				(builtin))
			(e-list
				(elems
					(e-num (value "1"))
					(e-num (value "2"))))
			(e-lambda
				(args
					(p-underscore))
				(e-record
					(fields
						(field (name "count")
							(e-num (value "1")))))))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "Config") (local)))))
	(d-let
		(p-assign (ident "omitted_mapped"))
		(e-call (constraint-fn-var 578)
			(e-lookup-external
				(builtin))
			(e-list
				(elems
					(e-num (value "1"))
					(e-num (value "2"))))
			(e-lambda
				(args
					(p-underscore))
				(e-empty_record)))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "Config") (local)))))
	(d-let
		(p-assign (ident "omitted_mixed"))
		(e-call (constraint-fn-var 585)
			(e-lookup-local
				(p-assign (ident "id")))
			(e-empty_record))
		(annotation
			(ty-lookup (name "Mixed") (local))))
	(s-nominal-decl
		(ty-header (name "Config"))
		(ty-record
			(field (field "count") (defaulted true)
				(ty-lookup (name "U8") (builtin)))
			(field (field "other") (defaulted true)
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Mixed"))
		(ty-record
			(field (field "label") (optional true)
				(ty-lookup (name "Str") (builtin)))
			(field (field "count") (defaulted true)
				(ty-lookup (name "U8") (builtin)))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 614)
					(e-lookup-local
						(p-assign (ident "total")))
					(e-record
						(fields
							(field (name "count")
								(e-num (value "1")))))))
			(rhs
				(e-num (value "1")))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 643)
					(e-lookup-local
						(p-assign (ident "total")))
					(e-nominal (nominal "Config")
						(e-empty_record))))
			(rhs
				(e-num (value "10")))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 662)
					(e-lookup-local
						(p-assign (ident "total")))
					(e-empty_record)))
			(rhs
				(e-num (value "10")))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-field-access
					(receiver
						(e-lookup-local
							(p-assign (ident "omitted"))))
					(segments
						(segment (name "count") (mode "required")))))
			(rhs
				(e-num (value "10")))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-field-access
					(receiver
						(e-lookup-local
							(p-assign (ident "omitted"))))
					(segments
						(segment (name "other") (mode "required")))))
			(rhs
				(e-num (value "20")))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-field-access
					(receiver
						(e-lookup-local
							(p-assign (ident "omitted_mixed"))))
					(segments
						(segment (name "count") (mode "required")))))
			(rhs
				(e-num (value "7")))))
	(s-expect
		(e-block
			(s-let
				(p-assign (ident "config"))
				(e-empty_record))
			(e-method-eq (negated "false")
				(lhs
					(e-field-access
						(receiver
							(e-lookup-local
								(p-assign (ident "config"))))
						(segments
							(segment (name "count") (mode "required")))))
				(rhs
					(e-num (value "10")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a"))
		(patt (type "Config -> U8"))
		(patt (type "Config"))
		(patt (type "Config"))
		(patt (type "List(Config)"))
		(patt (type "List(Config)"))
		(patt (type "List(Config)"))
		(patt (type "List(Config)"))
		(patt (type "Mixed")))
	(type_decls
		(nominal (type "Config")
			(ty-header (name "Config")))
		(nominal (type "Mixed")
			(ty-header (name "Mixed"))))
	(expressions
		(expr (type "a -> a"))
		(expr (type "Config -> U8"))
		(expr (type "Config"))
		(expr (type "Config"))
		(expr (type "List(Config)"))
		(expr (type "List(Config)"))
		(expr (type "List(Config)"))
		(expr (type "List(Config)"))
		(expr (type "Mixed"))))
~~~
