# META
~~~ini
description=Defaulted field literals-only rule from both sides: literal defaults accepted, a def-referencing default and a non-concrete default rejected
type=snippet
~~~
# SOURCE
~~~roc
Accepted := { n : U8 ?? 10, m : I8 ?? -1, s : Str ?? "hi", t : [Some(U8), None] ?? Some(1) }

accepted : Accepted
accepted = Accepted.{}

ten : U8
ten = 10

BadRef := { a : U8 ?? ten }

bad_ref : BadRef
bad_ref = BadRef.{ a: 1 }

Pair(x) := { items : List(x) ?? [] }

bad_list : Pair(U8)
bad_list = Pair.{}
~~~
# EXPECTED
DEFAULT VALUE MUST BE A LITERAL - record_default_literals.md:9:23:9:26
DEFAULT VALUE NOT CONCRETE - record_default_literals.md:14:33:14:35
# PROBLEMS
── ✗ default value must be a literal ─────────── record_default_literals.md:9:23

The default value for the a field is not a literal.

BadRef := { a : U8 ?? ten }
                      ^^^

A field default (??) is materialized by the compiler at every construction site
that omits the field, so it must be a literal: a number, an interpolation-free
string, a tag, or a list, record, or tuple built only from literals. Anything
that refers to another value could form an evaluation cycle the compiler will
not chase.

── ✗ default value not concrete ─────────────── record_default_literals.md:14:33

The default value for the items field does not have a concrete type.

Pair(x) := { items : List(x) ?? [] }
                                ^^

A default is evaluated once at compile time and filled in wherever construction
omits the field, so it must have exactly one runtime representation. Annotate
the field (or the default) with a concrete type.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,Comma,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,Comma,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,CloseSquare,OpDoubleQuestion,UpperIdent,NoSpaceOpenRound,Int,CloseRound,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,LowerIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpDoubleQuestion,OpenSquare,CloseSquare,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Accepted")
				(args))
			(ty-record
				(anno-record-field (name "n")
					(ty (name "U8"))
					(default
						(e-int (raw "10"))))
				(anno-record-field (name "m")
					(ty (name "I8"))
					(default
						(e-int (raw "-1"))))
				(anno-record-field (name "s")
					(ty (name "Str"))
					(default
						(e-string
							(e-string-part (raw "hi")))))
				(anno-record-field (name "t")
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "Some"))
								(ty (name "U8")))
							(ty (name "None"))))
					(default
						(e-apply
							(e-tag (raw "Some"))
							(e-int (raw "1")))))))
		(s-type-anno (name "accepted")
			(ty (name "Accepted")))
		(s-decl
			(p-ident (raw "accepted"))
			(e-nominal-record
				(mapper (e-tag (raw "Accepted")))
				(backing (e-record))))
		(s-type-anno (name "ten")
			(ty (name "U8")))
		(s-decl
			(p-ident (raw "ten"))
			(e-int (raw "10")))
		(s-type-decl
			(header (name "BadRef")
				(args))
			(ty-record
				(anno-record-field (name "a")
					(ty (name "U8"))
					(default
						(e-ident (raw "ten"))))))
		(s-type-anno (name "bad_ref")
			(ty (name "BadRef")))
		(s-decl
			(p-ident (raw "bad_ref"))
			(e-nominal-record
				(mapper (e-tag (raw "BadRef")))
				(backing (e-record
						(field (field "a")
							(e-int (raw "1")))))))
		(s-type-decl
			(header (name "Pair")
				(args
					(ty-var (raw "x"))))
			(ty-record
				(anno-record-field (name "items")
					(ty-apply
						(ty (name "List"))
						(ty-var (raw "x")))
					(default
						(e-list)))))
		(s-type-anno (name "bad_list")
			(ty-apply
				(ty (name "Pair"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "bad_list"))
			(e-nominal-record
				(mapper (e-tag (raw "Pair")))
				(backing (e-record))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "accepted"))
		(e-nominal (nominal "Accepted")
			(e-empty_record))
		(annotation
			(ty-lookup (name "Accepted") (local))))
	(d-let
		(p-assign (ident "ten"))
		(e-num (value "10"))
		(annotation
			(ty-lookup (name "U8") (builtin))))
	(d-let
		(p-assign (ident "bad_ref"))
		(e-nominal (nominal "BadRef")
			(e-record
				(fields
					(field (name "a")
						(e-num (value "1"))))))
		(annotation
			(ty-lookup (name "BadRef") (local))))
	(d-let
		(p-assign (ident "bad_list"))
		(e-nominal (nominal "Pair")
			(e-empty_record))
		(annotation
			(ty-apply (name "Pair") (local)
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Accepted"))
		(ty-record
			(field (field "n") (defaulted true)
				(ty-lookup (name "U8") (builtin)))
			(field (field "m") (defaulted true)
				(ty-lookup (name "I8") (builtin)))
			(field (field "s") (defaulted true)
				(ty-lookup (name "Str") (builtin)))
			(field (field "t") (defaulted true)
				(ty-tag-union
					(ty-tag-name (name "Some")
						(ty-lookup (name "U8") (builtin)))
					(ty-tag-name (name "None"))))))
	(s-nominal-decl
		(ty-header (name "BadRef"))
		(ty-record
			(field (field "a")
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Pair")
			(ty-args
				(ty-rigid-var (name "x"))))
		(ty-record
			(field (field "items") (defaulted true)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var-lookup (ty-rigid-var (name "x"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Accepted"))
		(patt (type "U8"))
		(patt (type "BadRef"))
		(patt (type "Pair(U8)")))
	(type_decls
		(nominal (type "Accepted")
			(ty-header (name "Accepted")))
		(nominal (type "BadRef")
			(ty-header (name "BadRef")))
		(nominal (type "Pair(x)")
			(ty-header (name "Pair")
				(ty-args
					(ty-rigid-var (name "x"))))))
	(expressions
		(expr (type "Accepted"))
		(expr (type "U8"))
		(expr (type "BadRef"))
		(expr (type "Pair(U8)"))))
~~~
